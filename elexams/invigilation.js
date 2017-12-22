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
            `<table>
              <tr>
                <td>
                  <table>
                    <tr><td class="invigilators">Verfügbare Aufsichten</td></tr>
                    <tr><td class="invigilators">Verfügbare Aufsichten</td></tr>
                  </table>
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
              output += `">
                            ${slotsPerDay[i]}<br>
                            ${reserveInvigilator}
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
                  output += `" onclick="openModal('modal-${exam.anCode}-${room.roomID}')">
                                ${exam.anCode}. ${exam.name}<br>
                                Prüfer: ${exam.lecturer.personShortName}<br>
                                ${room.roomID} (${room.studentsInRoom.length}/${room.maxSeats}):
                                ${invigilator.invigilatorName}<br>
                                ${exam.duration + room.deltaDuration} Minuten`
                  if (room.reserveRoom) {
                    output += ` <span class="Reserve">(Reserve)</span>`
                  }
                  if (room.handicapCompensation) {
                    output += ` <span class="NTA">(NTA)</span>`
                  }
                  output += `</div></td>`
                  modalOutput += `<div id="modal-${exam.anCode}-${room.roomID}" class="modal">
                                    <div class="modal-content">
                                      <span id="modal-${exam.anCode}-${room.roomID}-close">&times;</span>
                                      <h2>${room.roomID}: ${exam.anCode}. ${exam.name}, ${exam.lecturer.personShortName}`
                  if (room.reserveRoom) {
                    modalOutput += ` <span class="Reserve">(Reserve)</span>`
                  }
                  if (room.handicapCompensation) {
                    modalOutput += ` <span class="NTA">(NTA)</span>`
                  }
                  modalOutput += `</h2><ol class="studentList">`
                  for (let s in room.studentsInRoom) {
                    const student = room.studentsInRoom[s]
                    modalOutput += `<li> ${student.studentName}`
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
          output += '</table></td></tr></table>'
          output += modalOutput
          $('#invigilations' + dayIndex).html(output)
        })
      },
      contentType: 'application/json',
      dataType: 'json'
    })
  }
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
  //   let output = '<ol class="invigilatorList">'
  //   for (let i in invigilators) {
  //     const invigilator = invigilators[i]
  //     output += `<li class="invigilatorList">${invigilator.invigilatorName}: ${invigilator.invigilatorPerson}</li>`
  //   }
  //   output += '</ol>'
  //   $('#invigilators').html(output)
  // })
  // Aufsichten anzeigen
    let output =
      `<table id="invigilatorList" class="invigilatorList" >
      <thead>
      <tr class="invigilatorList">
        <th class="invigilatorList">Name</th>
        <th class="invigilatorList">Prüfungstage</th>
        <th class="invigilatorList">ausgeschlossene Tage</th>
        <th class="invigilatorList">gewünschte Tage</th>
        <th class="invigilatorList">mögliche Tage</th>
        <th class="invigilatorList">zu leistende Zeit</th>
        <th class="invigilatorList">eingeplante Zeit</th>
        <th class="invigilatorList">noch offen</th>
     </tr>
     </thead>
     <tbody>`
    for (let i in invigilators) {
      let invigilator = invigilators[i]
      output += `<tr class="invigilatorList">
             <td class="invigilatorList">${invigilator.invigilatorName}</td>
             <td class="invigilatorList">${invigilator.invigilatorExamDays}</td>
             <td class="invigilatorList">${invigilator.invigilatorExcludedDays}</td>
             <td class="invigilatorList">${invigilator.invigilatorWantDays}</td>
             <td class="invigilatorList">${invigilator.invigilatorCanDays}</td>
             <td class="invigilatorList">${invigilator.invigilatorMinutesTodo}</td>
             <td class="invigilatorList">${invigilator.invigilatorsMinutesPlanned}</td>
             <td class="invigilatorList">${invigilator.invigilatorMinutesTodo - invigilator.invigilatorsMinutesPlanned}</td> </tr>`
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
