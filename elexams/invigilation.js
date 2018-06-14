let fetchInvigilations = () => {
  $.getJSON(endpoints.examDays, function (examDays) {
    let output = '<div class="invigtab">'
    output += `<button class="invigtablinks"
                onclick="openInvigilation(event, -1)">Aufsichten</button>`
    for (let i in examDays) {
      const examDay = examDays[i].substring(5)
      output += `<button class="invigtablinks"
                  onclick="openInvigilation(event, ${i})">${examDay}</button>`
    }
    output += '</div>'
    output += `<div id="invig-1" class="invigtabcontent">
      <div id="invigilators"></div>
    </div>`
    for (let i in examDays) {
      const examDay = examDays[i].substring(5)
      output += `<div id="invig${i}" class="invigtabcontent">
        <div id="invigilations${i}"></div>
      </div>`
    }
    $('#invigilations').html(output)
  })
}

const openInvigilation = (evt, dayIndex) => {
  // Get all elements with class="tabcontent" and hide them
  const tabcontent = document.getElementsByClassName('invigtabcontent')
  for (let i = 0; i < tabcontent.length; i++) {
      tabcontent[i].style.display = 'none'
  }

  // Get all elements with class="tablinks" and remove the class "active"
  const tablinks = document.getElementsByClassName('invigtablinks');
  for (let i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(' active', '')
  }

  // Show the current tab, and add an "active" class to the button that opened the tab
  document.getElementById('invig' + dayIndex).style.display = 'block'
  evt.currentTarget.className += ' active'
  openInvigilationContent(dayIndex)
}

const openInvigilationContent = (dayIndex) => {
  if (dayIndex === -1) {
    fetchInvigilators()
  } else {
    $.ajax({
      type: 'POST',
      url: endpoints.slotsForDay,
      data: JSON.stringify(dayIndex),
      success: (slots) => {
        $.getJSON(endpoints.slotsPerDay, function (slotsPerDay) {
          let modalOutput = ''
          let output =
            `<table class="invigilators">
              <tr>
                <td>
                 <h3 class="invigilators">Verfügbare Aufsichten (Want)</h3>
                 <div id="invigilatorWantDays-${dayIndex}"></div>
                <td>
                <td>
                  <table>
                `
          for (let i in slotsPerDay) {
            let reserveInvigilator = _fetchReserve(dayIndex, i, slots)
            const examData = _fetchExamsData(dayIndex, i, slots)
            if (examData.length > 0) {
              output += `<tr>
                <td><div id="invigilation-slot-${dayIndex}-${i}" class="invigilation`
              if (reserveInvigilator === null) {
                reserveInvigilator = 'Reserve fehlt'
                output += ' needsInvigilator'
              } else {
                output += ' hasInvigilator'
              }
              output += `" ondrop="dropInvigilator(event, ${dayIndex}, ${i}, null)"
                          ondragover="allowDrop(event)">
                            ${slotsPerDay[i]}<br>
                            ${reserveInvigilator.invigilatorName}
                          </div></td>`
              for (let j in examData) {
                const exam = examData[j]
                for (let k in exam.rooms) {
                  const room = exam.rooms[k]
                  let invigilator
                  output += `<td><div class="invigilation`
                  if (room.invigilator === null) {
                    invigilator = 'Aufsicht fehlt'
                    output += ' needsInvigilator'
                  } else {
                    invigilator = room.invigilator
                    output += ' hasInvigilator'
                  }
                  output += `" onclick="openModal('modal-${exam.anCode}-${room.roomID}-${room.studentsInRoom.length}')"
                                ondrop="dropInvigilator(event, ${exam.slot}, '${room.roomID}')" ondragover="allowDrop(event)">
                                ${exam.anCode}. ${exam.name}<br>
                                Prüfer: ${exam.lecturer.personShortName}<br>
                                <span class="room-${room.roomID}">${room.roomID}</span>
                                (${room.studentsInRoom.length}/${room.maxSeats}):
                                    ${invigilator.invigilatorName} <br>
                                ${exam.duration + room.deltaDuration} Minuten`
                  if (room.reserveRoom) {
                    output += ` <span class="Reserve">(Reserve)</span>`
                  }
                  if (room.handicapCompensation) {
                    output += ` <span class="NTA">(NTA)</span>`
                  }
                  output += `</div></td>`
                  modalOutput += `<div id="modal-${exam.anCode}-${room.roomID}-${room.studentsInRoom.length}" class="modal">
                                    <div class="modal-content">
                                      <span id="modal-${exam.anCode}-${room.roomID}-${room.studentsInRoom.length}-close">&times;</span>
                                      <h2>${room.roomID}: ${exam.anCode}. ${exam.name}, ${exam.lecturer.personShortName}`
                  if (room.reserveRoom) {
                    modalOutput += ` <span class="Reserve">(Reserve)</span>`
                  }
                  if (room.handicapCompensation) {
                    modalOutput += ` <span class="NTA">(NTA)</span>`
                  }
                  modalOutput += `</h2>`
                  if (room.invigilator !== null) {
                    modalOutput +=
                      `<h3>Aufsicht: ${room.invigilator.invigilatorID}. ${room.invigilator.invigilatorName}
                      <span id="remove-${exam.anCode}-${room.roomID}-${room.invigilator.invigilatorID}">
                      entfernen</span>
                      </h3>`
                  }
                  modalOutput += `<ol class="studentList">`
                  for (let s in room.studentsInRoom) {
                    const student = room.studentsInRoom[s]
                    modalOutput += `<li> ${student.studentFamilyname}, ${student.studentFirstname}`
                    if (student.studentHandicap !== null) {
                      modalOutput += ` (${student.studentHandicap.handicapCompensationText})`
                    }
                    modalOutput += `</li>`
                  }
                  modalOutput += `</ol></div></div>`
                }
              }
              output += `</tr>`
            }
          }
          output += `</table></td><td>
                           <h3 class="invigilators">Verfügbare Aufsichten (Can)</h3>
                           <div id="invigilatorCanDays-${dayIndex}"></div>
                          <td></tr></table>`
          output += modalOutput
          $('#invigilations' + dayIndex).html(output)
        })
        $.ajax({
          type: 'POST',
          url: endpoints.invigilatorsForDay,
          data: JSON.stringify(dayIndex),
          success: (invigs) => {
            for (let i in invigs) { // 0 == want, 1 == can
              let wantInvigs = true
              if (i == 1) {
                wantInvigs = false
              }
              let output = `<ol class="`
              if (wantInvigs) {
                output += `wantInvigs`
              } else {
                output += `canInvigs`
              }
              output += `">`
              let invigilators = invigs[i]
              for (let j in invigilators) {
                let invig = invigilators[j]
                output += `<li class="invigilatorListItem">
                          <div id="invigilator-${invig.invigilatorID}" class="invigilator `
                if (wantInvigs) {
                  output += `wantInvig`
                } else {
                  output += `canInvig`
                }
                output += `"
                          draggable="true" ondragstart="dragInvigilatorStart(event, ${invig.invigilatorID})">
                          <span class="invigName">${invig.invigilatorName}</span> (${invig.invigilatorID})<br>offen:`
                if (invig.invigilatorMinutesTodo - invig.invigilatorsMinutesPlanned < 0) {
                  output += '<span class="invigilationNegativ">'
                } else {
                  output += '<span class="invigilationPositiv">'
                }
                output += `${invig.invigilatorMinutesTodo - invig.invigilatorsMinutesPlanned} Minuten</span><br>
                          geplante Tage: ${invig.invigilatorInvigilationDays}<br>
                          Wunschtage: ${invig.invigilatorWantDays}
                          </div>
                          </li>`
              }
              output += `</ol>`
              if (wantInvigs) {
                $('#invigilatorWantDays-' + dayIndex).html(output)
              } else {
                $('#invigilatorCanDays-' + dayIndex).html(output)
              }
            }
          },
          contentType: 'application/json',
          dataType: 'json'
        })
      },
      contentType: 'application/json',
      dataType: 'json'
    })
  }
}

const dragInvigilatorStart = (event, invigilatorID) => {
  event.dataTransfer.setData("InvigilatorID", invigilatorID);
}

const addInvigilatorToExamOrSlot = (invigilatorID, dayIdx, slotIdx, room) => {
  let result = false
  const dataToTransfer = JSON.stringify({
    addInvigilatorID: invigilatorID,
    addInvigilatorSlot: [dayIdx, slotIdx],
    addInvigilatorRoom: room
  })
  // alert(dataToTransfer)

  $.ajax({
    type: 'POST',
    url: endpoints.addInvigilator,
    data: dataToTransfer,
    success: function () {
      openInvigilationContent(dayIdx)
      result = true
    },
    contentType: 'application/json',
    dataType: 'json'
  })
  return result
}

const dropInvigilator = (event, dayIdx, slotIdx, room) => {
  event.preventDefault();
  const invigilatorID = parseInt(event.dataTransfer.getData("InvigilatorID"))
  const dropped = addInvigilatorToExamOrSlot(invigilatorID, dayIdx, slotIdx, room)
  // alert(`addInvigilatorToExamOrSlot(${invigilatorID}, ${dayIdx}, ${slotIdx}, ${room})`)
}

function allowDrop(event) {
    event.preventDefault();
}

const openModal = (modalid) => {
  const modal = document.getElementById(modalid)
  const span = document.getElementById(modalid + '-close')
  modal.style.display = 'block'
  span.onclick = () => {
    modal.style.display = 'none'
  }
  window.onclick = (event) => {
    if (event.target === modal) {
      modal.style.display = 'none'
    }
  }
}

let fetchInvigilators = () => {
  $.getJSON(endpoints.invigilators, (invigilators) => {
    let output =
      `<ul class="invigilations">
        <li class="invigilations">Summe Aufsichten Prüfungen: ${invigilators[0].invigilationsSumExamRooms} Minuten</li>
        <li class="invigilations">Summe Reserveaufsichten: ${invigilators[0].invigilationsSumReserve} Minuten</li>
        <li class="invigilations">Summe Beisitz mündliche Prüfungen: ${invigilators[0].invigilationsSumOralExams} Minuten</li>
        <li class="invigilations">Summe Mastergespräche: ${invigilators[0].invigilationsSumMaster} Minuten</li>
        <li class="invigilations">Summe Aufsichten Livecoding: ${invigilators[0].invigilationsSumLivecoding} Minuten</li>
        </ul>
      <table id="invigilatorList" class="invigilatorList" >
      <thead>
      <tr class="invigilatorList">
        <th class="invigilatorList">Nr.</th>
        <th class="invigilatorList">ID</th>
        <th class="invigilatorList">Name</th>
        <th class="invigilatorList">Prüfungstage</th>
        <th class="invigilatorList">gewünschte Tage</th>
        <th class="invigilatorList">mögliche Tage</th>
        <th class="invigilatorList">ausgeschlossene Tage</th>
        <th class="invigilatorList">geplante Tage</th>
        <th class="invigilatorList">zu leistende Zeit</th>
        <th class="invigilatorList">eingeplante Zeit</th>
        <th class="invigilatorList">noch offen</th>
        <th class="invigilatorList">Teilzeit</th>
        <th class="invigilatorList">Beisitzer</th>
        <th class="invigilatorList">Master</th>
        <th class="invigilatorList">Livecoding</th>
        <th class="invigilatorList">mehr im letzten Semester</th>
        <th class="invigilatorList">mehr dieses Semester</th>
        <th class="invigilatorList">Freisemester</th>
     </tr>
     </thead>
     <tbody>`
    for (let i in invigilators[1]) {
      let invigilator = invigilators[1][i]
      output +=
           `<tr class="invigilatorList">
             <td class="invigilatorList">${i}</td>
             <td class="invigilatorList">${invigilator.invigilatorID}</td>
             <td class="invigilatorList">${invigilator.invigilatorName}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorExamDays}">${invigilator.invigilatorExamDays}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorWantDays}">${invigilator.invigilatorWantDays}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorCanDays}">${invigilator.invigilatorCanDays}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorExcludedDays}">${invigilator.invigilatorExcludedDays}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorInvigilationDays}">${invigilator.invigilatorInvigilationDays}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorMinutesTodo}">${invigilator.invigilatorMinutesTodo}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorsMinutesPlanned}">${invigilator.invigilatorsMinutesPlanned}</td>`
      if (invigilator.invigilatorMinutesTodo - invigilator.invigilatorsMinutesPlanned < 0) {
        output += '<td class="invigilatorList invigilationNegativ">'
      } else {
        output += '<td class="invigilatorList invigilationPositiv">'
      }
      output += `${invigilator.invigilatorMinutesTodo - invigilator.invigilatorsMinutesPlanned}</td>
             <td class="invigilatorList invigilatorListContentPartime${invigilator.invigilatorPartTime}">${invigilator.invigilatorPartTime}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorOralExams}">${invigilator.invigilatorOralExams}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorMaster}">${invigilator.invigilatorMaster}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorLiveCoding}">${invigilator.invigilatorLiveCoding}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorOvertimeLastSemester}">${invigilator.invigilatorOvertimeLastSemester}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorOvertimeThisSemester}">${invigilator.invigilatorOvertimeThisSemester}</td>
             <td class="invigilatorList invigilatorListContent${invigilator.invigilatorFreeSemester}">${invigilator.invigilatorFreeSemester}</td>
           </tr>`
    }
    output += `</tbody>
              </table>`
    $('#invigilators').html(output)
    $('#invigilatorList').tablesorter({
      sortList: [
        [0, 0]
      ]
    })
  }).fail(function (jqXHR, textStatus, errorThrown) {
    $('#error').append(`Error on endpoint \\exams: `)
    $('#error').append(jqXHR.responseText)
    $('#error').append(`<br>`)
    $('#error').css({
      'border': '3px solid #e22d2d'
    })
  })
}
