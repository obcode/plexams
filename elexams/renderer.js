// Backend and endpoint details
const host = 'http://127.0.0.1:8080'
const endpointExams = '/exams'
const endpointExamDays = '/examDays'
const endpointSlots = '/slots'
const endpointSlotsPerDay = '/slotsPerDay'
const endpointAddExam = '/addExam'
const endpointOverlaps = '/overlaps'
const endpointUnscheduledExams = '/unscheduledExams'
const endpointNotPlannedByMeExams = '/notPlannedByMeExams'
const endpointValidation = '/validation'
const endpointExamsBySameLecturer = '/examsBySameLecturer'
const endpointGoSlots = '/goSlots'

let _fetchValidation = () => {
  $.getJSON(host + endpointValidation, (validation) => {
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

let _fetchExams = function () {
  $.getJSON(host + endpointExams, function (exams) {
    let output =
      `<table id="examList" class="examList" >
      <thead>
      <tr class="examList">
        <th class="examList">Prüfung</th>
        <th class="examList">Prüfer</th>
        <th class="examList">Anmeldecode</th>
        <th class="examList">Dauer</th>
        <th class="examList">Wiederholungsklausur</th>
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
             </tr>`
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
  $.getJSON(host + endpointUnscheduledExams, function (uExams) {
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
  $.getJSON(host + endpointNotPlannedByMeExams, function (uExams) {
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

let _fetchExamsData = function (inDay, inTime, slots) {
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
        let exam = examsInSlot[anCode]
        if (anCode == null) {
          anCode = ''
        }
        let name = ''
        if (exam != null) {
          name = exam.name
        }
        arr.push(anCode + `</br>` + name)
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
  $.getJSON(host + endpointExamDays, function (examDays) {
      $.getJSON(host + endpointSlotsPerDay, function (slotsPerDay) {
        $.getJSON(host + endpointSlots, function (slots) {
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
                output += `<div id="${anCodes[k]}" class="inner" ondrop="return false;"
                            draggable="true" ondragstart="dragExam(event)"
                            onclick="viewDetails(event, ${anCodes[k]})">${examData[k]}
                            </div>`
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
              </tr>
            </table>
            </br>`
          $('#plan').html(output)
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
  $.getJSON(host + endpointNotPlannedByMeExams, function (exams) {
    for (var i in exams) {
      let ancode = exams[i].anCode
      $('#'.concat(ancode)).addClass('notPlannedByMe')
    }
  })
}


function toggleGoSlots () {
  $.getJSON(host + endpointGoSlots, (goSlots) => {
    for (let i in goSlots) {
      let goSlot = goSlots[i]
      $(['#slot_', goSlot[0], '_', goSlot[1]].join('')).addClass('goSlot')
    }
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

// Start to fetch the exam list
fetchExams()

// Start to fetch the exam days
fetchExamDays()

// Start to fetch the unscheduled exams
fetchUnscheduledExams()

_fetchNotPlannedByMeExams()

setNotPlannedByMe()

_fetchValidation()
