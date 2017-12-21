// Backend and endpoint details
const host = 'http://127.0.0.1:8080'
const endpoints =
  { exams: host + '/exams',
    examDays: host + '/examDays',
    slots: host + '/slots',
    slot: host + '/slot',
    slotsPerDay: host + '/slotsPerDay',
    addExam: host + '/addExam',
    overlaps: host + '/overlaps',
    unscheduledExams: host + '/unscheduledExams',
    notPlannedByMeExams: host + '/notPlannedByMeExams',
    validation: host + '/validation',
    validateWhat: host + '/validateWhat',
    examsBySameLecturer: host + '/examsBySameLecturer',
    goSlots: host + '/goSlots',
    lecturer: host + '/lecturer',
    reloadPlan: host + '/reloadPlan',
    invigilators: host + '/invigilators'
  }


const openTab = (evt, tabname) => {

  // Get all elements with class="tabcontent" and hide them
  const tabcontent = document.getElementsByClassName('tabcontent')
  for (let i = 0; i < tabcontent.length; i++) {
      tabcontent[i].style.display = 'none'
  }

  // Get all elements with class="tablinks" and remove the class "active"
  const tablinks = document.getElementsByClassName('tablinks');
  for (let i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(' active', '')
  }

  // Show the current tab, and add an "active" class to the button that opened the tab
  document.getElementById(tabname).style.display = 'block'
  evt.currentTarget.className += ' active'
  if (tabname === 'Prüfungsplanung') {
    $('#plan').html('<h1>Generating...</h1>')
    fetchExamDays()
    fetchUnscheduledExams()
    // _fetchValidateWhat()
  } else if (tabname === 'Aufsichtenplanung') {
    fetchInvigilations()
  } else if (tabname === 'Validation') {
    $('#validation-full').html('<h1>Validating...</h1>')
    fetchValidation()
  } else if (tabname === 'Prüfungsliste') {
    fetchExams()
  } else if (tabname === 'ReloadPlan') {
    reloadPlan()
  }
}

const reloadPlan = () => {
  $('#reloadedPlan').html('<h1>Loading...</h1>')
  $.getJSON(endpoints.reloadPlan, (reloadResult) => {
    let output = ''
    if (reloadResult[0]) {
      output += '<h1>Plan neu geladen</h1>'
    } else {
      output += '<h1><span class="error">Fehler beim Plan neu laden</span></h1>'
    }
    const errorMessages = reloadResult[1]
    if (errorMessages.length > 0) {
      output += '<ul>'
      for (let i in errorMessages) {
        const errorMessage = errorMessages[i]
        output += `<li>${errorMessage}</li>`
      }
      output += '</ul>'
    }
    $('#reloadedPlan').html(output)
  })
}

let _fetchValidateWhat = () => {
  $.getJSON(endpoints.validateWhat, (validateWhat) => {
    let output = 'Validation: <form>'
    for (let i in validateWhat) {
      output += `<input type="checkbox" name="${validateWhat[i]}"
                                        value="${validateWhat[i]}">
                 ${validateWhat[i]} | `
    }
    output += '</form>'
    $('#validateWhat').html(output)
  })
}

let _fetchValidation = () => {
  $.getJSON(endpoints.validation, (validation) => {
    let output = `<h1>${validation.result}</h1><ul>`
    for (let i in validation.brokenConstraints) {
      let constraint = validation.brokenConstraints[i]
      if (constraint.tag === 'HardConstraintBroken') {
        output += `<li><span class="${constraint.tag}">
                    ${constraint.contents}</span>
                  </li>`
      }
    }
    output += `</ul>`
    $('#validation').html(output)

    output = `<h1>${validation.result}</h1><ul>`
    for (let i in validation.brokenConstraints) {
      let constraint = validation.brokenConstraints[i]
      output += `<li><span class="${constraint.tag}">
                  ${constraint.contents}</span>
                </li>`
    }
    output += `</ul>`
    $('#validation-full').html(output)

  })
}

let fetchValidation = () => {
  var request = $.ajax({
    type: 'POST',
    url: endpoints.validation,
    data: JSON.stringify(["ValidateSchedule"]),
    contentType: 'application/json',
    dataType: 'json'
  })
  request.done(function (validation) {
    output = `<h1>${validation.result}</h1><ul>`
    for (let i in validation.brokenConstraints) {
      let constraint = validation.brokenConstraints[i]
      output += `<li><span class="${constraint.tag}">
                  ${constraint.contents}</span>
                </li>`
    }
    output += `</ul>`
    $('#validation-full').html(output)
  })
}

let _fetchExams = function () {
  $.getJSON(endpoints.exams, function (exams) {
    let output =
      `<table id="examList" class="examList" >
      <thead>
      <tr class="examList">
        <th class="examList">Prüfung</th>
        <th class="examList">Prüfer</th>
        <th class="examList">Anmeldecode</th>
        <th class="examList">Dauer</th>
        <th class="examList">Wiederholungsklausur</th>
        <th class="examList">Tag</th>
        <th class="examList">Gruppen</th>
     </tr>
     </thead>
     <tbody>`
    for (let i in exams) {
      let exam = exams[i]
      output += `<tr class="examList">
             <td class="examList">${exam.name}</td>
             <td class="examList">${exam.lecturer.personShortName}</td>
             <td class="examList">${exam.anCode}</td>
             <td class="examList">${exam.duration}</td>
             <td class="examList">${exam.reExam}</td>
             <td class="examList">${exam.slot ? exam.slot : '-'}</td>
             <td class="examList">`
      for (let g in exam.registeredGroups) {
        let group = exam.registeredGroups[g]
        output += `${group.registeredGroupDegree}(${group.registeredGroupStudents}), `
      }
      output += `</td> </tr>`
    }
    output += `</tbody>
              </table>`
    $('#plexams-api').html(output)
    $('#examList').tablesorter({
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

let _fetchUnscheduledExams = function () {
  $.getJSON(endpoints.unscheduledExams, function (uExams) {
    let outputPlannedByMe = ``
    for (var i in uExams) {
      var exam = uExams[i]
      var draggable = false
      outputPlannedByMe +=
        `<div id="${exam.anCode}" `
      if (exam.plannedByMe) {
        outputPlannedByMe += `class="innerUnscheduled" draggable="true"`
      } else {
        outputPlannedByMe += `class="innerUnscheduled notPlannedByMe" draggable="false"`
      }
      outputPlannedByMe += ` ondrop="return false;" ondragstart="dragExam(event)"
          onclick="viewDetails(event, ${exam.anCode})"
         >${exam.anCode}</br>${exam.name}</div>`
    }
    $('#unscheduled').html(outputPlannedByMe)
  }).fail(function (jqXHR, textStatus, errorThrown) {
    $('#error').append(`Error on endpoint \\unscheduledExams: `)
    $('#error').append(jqXHR.responseText)
    $('#error').append(`<br>`)
    $('#error').css({
      'border': '3px solid #e22d2d'
    })
  })
}

let _fetchNotPlannedByMeExams = function () {
  $.getJSON(endpoints.notPlannedByMeExams, function (uExams) {
    let outputNotPlannedByMe = ``
    for (var i in uExams) {
      var exam = uExams[i]
      var draggable = false

      outputNotPlannedByMe +=
        `<div id="${exam.anCode}" class="innerUnNotPbyMe" ondrop="return false;"
          draggable="false" onclick="viewDetails(event, ${exam.anCode})"
          >${exam.anCode}</br>${exam.name}</div>`
    }
    $('#notPlannedByMe').html(outputNotPlannedByMe)
  }).fail(function (jqXHR, textStatus, errorThrown) {
    $('#error').append(`Error on endpoint \\unscheduledExams: `)
    $('#error').append(jqXHR.responseText)
    $('#error').append(`<br>`)
    $('#error').css({
      'border': '3px solid #e22d2d'
    })
  })
}

let _fetchReserve = function (inDay, inTime, slots) {
  for (let i in slots) {
    const slot = slots[i]
    const timeSlot = slot[0]
    const slotValue = slot[1]
    const day = timeSlot[0]
    const time = timeSlot[1]
    if (day == inDay && time == inTime) {
      return slotValue.reserveInvigilator
    }
  }
}


let _fetchExamsData = function (inDay, inTime, slots) {
  for (var i in slots) {
    let slot = slots[i]
    let timeSlot = slot[0]
    let slotValue = slot[1]
    let day = timeSlot[0]
    let time = timeSlot[1]
    if (day == inDay && time == inTime) {
      let examsInSlot = slotValue.examsInSlot
      var arr = []
      for (var j in Object.keys(examsInSlot)) {
        let anCode = Object.keys(examsInSlot)[j]
        let exam = examsInSlot[anCode]
        if (anCode == null) {
          anCode = ''
        }
        let name = ''
        let reExam = ''
        if (exam != null) {
          name = exam.name
          if (exam.reExam) {
            reExam = '(W)'
          }
        }
//        arr.push(anCode + reExam + `</br>` + name + `</br>`)
        arr.push(exam)
        //   {
        //   anCode: exam.anCode,
        //   reExam: exam.reExam,
        //   name: exam.name,
        //   lecturer: exam.lecturer.personShortName
        // })
      }
      return arr
    }
  }
}
let _getAncodesForSlot = function (inDay, inTime, slots) {
  for (var i in slots) {
    let slot = slots[i]
    let timeSlot = slot[0]
    let exams = slot[1]
    let day = timeSlot[0]
    let time = timeSlot[1]
    if (day == inDay && time == inTime) {
      let examsInSlot = exams.examsInSlot
      var arr = []

      for (var j in Object.keys(examsInSlot)) {
        let anCode = Object.keys(examsInSlot)[j]
        if (anCode == null) {
          anCode = ''
        }
        arr.push(anCode)
      }
      return arr
    }
  }
}
let _fetchExamDescription = function (inDay, inTime, slots) {
  var description = 'text'

  return description;
}

let _fetchExamDays = function () {
  $.getJSON(endpoints.examDays, function (examDays) {
      $.getJSON(endpoints.slotsPerDay, function (slotsPerDay) {
        $.getJSON(endpoints.slots, function (slots) {
          // Construct the plan output
          let output =
            `<table>
                    <tr>
                    <td>
                      <table>
                        <tr>
                          <th></th>`
          for (let i in examDays) {
            let examDay = examDays[i]
            output += `<th>${examDay}</th>`
          }
          output += `</tr>`

          for (let i in slotsPerDay) {
            let slot = slotsPerDay[i]
            output += `<tr>
                          <td class="times">${slot}</td>`
            for (let j in examDays) {
              let examDay = examDays[j]
              let examData = _fetchExamsData(j, i, slots)
              var anCodes = _getAncodesForSlot(j, i, slots);
              output += `<td class="exams">
                        <div id="slot_${j}_${i}" class="outer" data-day="${j}" data-slot="${i}"
                        ondrop="dropExam(event)" ondragover="allowDropExam(event)">`
              for (let k in examData) {
                const exam = examData[k]
                output += `<div id="${exam.anCode}" class="inner `
                if (exam.reExam) {
                  output += 'reExam'
                }
                output += `" ondrop="return false;"
                           draggable="true" ondragstart="dragExam(event)"
                           onclick="viewDetails(event, ${exam.anCode})"
                           title="${exam.anCode}"
                           onmouseover="">
                           ${exam.anCode}. `
                for (let g in exam.registeredGroups) {
                  const group = exam.registeredGroups[g]
                  output += `${group.registeredGroupDegree}(${group.registeredGroupStudents}),`
                }
                output += `<br>
                           <span class="examName">${exam.name}</span><br>
                           ${exam.lecturer.personShortName}<br>`
                for (let r in exam.rooms) {
                  output += `${exam.rooms[r].roomID}, `
                }
                output += `</div>`
              }
              output += `</div>
                        </td>`
            }
            output += `</tr>`
          }
          // Detailed description of the selected exams
          output += `</tr>
                </table>
                </td>
                <td style="vertical-align:top; word-break: break-word; width:15em;">
                  <div id="description">
                  </div>
                </td>
                </td>
              </tr>
            </table>
            </br>`
          $('#plan').html(output)
          toggleGoSlots()
          setNotPlannedByMe()
        })
      })
  })
    .fail(function (jqXHR, textStatus, errorThrown) {
      $('#error').append(`Error on endpoint \\examDays: `)
      $('#error').append(jqXHR.responseText)
      $('#error').append(`<br>`)
      $('#error').css({
        'border': '3px solid #e22d2d'
      })
    })
}

function setNotPlannedByMe () {
  $.getJSON(endpoints.notPlannedByMeExams, function (ancodes) {
    for (var i in ancodes) {
      let ancode = ancodes[i]
      $('#'.concat(ancode)).addClass('notPlannedByMe')
    }
  })
}


function toggleGoSlots () {
  $.getJSON(endpoints.goSlots, (goSlots) => {
    for (let i in goSlots) {
      let goSlot = goSlots[i]
      $(['#slot_', goSlot[0], '_', goSlot[1]].join('')).addClass('goSlot')
    }
  })
}

function _fetchLecturer () {
  $.getJSON(endpoints.lecturer, (lecturers) => {
    let output = '<ul id="lecturerlist">'
    for (let i in lecturers) {
      const lecturer = lecturers[i]
      output += `<li>
          <span id="lecturer_${lecturer.personID}" class="lecturer"
          onclick="viewExams(event, ${lecturer.personID})"> ${lecturer.personShortName} `
      if (lecturer.personIsLBA) {
        output += '(LBA)'
      } else {
        output += `(${lecturer.personFK})`
      }
      output += `</span></li>`
    }
    output += '</ul>'
    $('#lecturer').html(output)
  })
}

// Convenience function for _fetchExams
let fetchExams = function () {
  _fetchExams()
}

// Convenience function for _fetchExamDays
let fetchExamDays = function () {
  _fetchExamDays()
}

// Convenience function for _fetchExamDays
let fetchUnscheduledExams = function () {
  _fetchUnscheduledExams()
}

// // Start to fetch the exam list
// fetchExams()
//
// // Start to fetch the exam days
// fetchExamDays()
//
// // Start to fetch the unscheduled exams
// fetchUnscheduledExams()
//
// // _fetchNotPlannedByMeExams()
//
// _fetchValidateWhat()
//
// setNotPlannedByMe()

// _fetchLecturer()

// _fetchValidation()
