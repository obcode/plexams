// Backend and endpoint details
const host = 'http://127.0.0.1:8080';
const endpointExams = '/exams';
const endpointExamDays = '/examDays';
const endpointSlots = '/slots';
const endpointSlotsPerDay = '/slotsPerDay';
const endpointUnscheduledExams = '/unscheduledExams';

// Retry configuration
let maxNoOfAttempts = 50,
  waitTimeBetweenAttempt = 250;

let _fetchExams = function (waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + endpointExams, function (exams) {
    // Construct the user list HTML output
  let output =
    `<table style="border: 1px solid black;">
      <tr>
        <th>Prüfung</th>
        <th>Prüfer</th>
        <th>Anmeldecode</th>
        <th>Wiederholungsklausur</th>
     </tr>`;
  for(let i in exams) {
    let exam = exams[i];
    output += `<tr>
             <td>${exam.name}</td>
             <td>${exam.lecturer.personShortName}</td>
             <td>${exam.anCode}</td>
             <td>${exam.reExam}</td>
             </tr>`;
  }
  output += `</table>`;
    $('#plexams-api').html(output);
  }).fail(function () {
    $.ajax(host + endpointExams).fail(function(jqXHR, textStatus, errorThrown) {
      alert(jqXHR.responseText);
      $('#error').html(jqXHR.responseText);
    })
  });
};

let _fetchUnscheduledExams = function (waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + endpointUnscheduledExams, function (uExams) {
    // Construct the user list HTML output
    let output =``
    for(var i in uExams){
      var exam = uExams[i];
      output += `<div id="${exam.anCode}" class="innerUnscheduled" ondrop="return false;"
                draggable="true" ondragstart="dragExam(event)" onclick="viewDetails(${exam.anCode})">${exam.anCode}</br>${exam.name}</div>`;
    }
    $('#unscheduled').html(output);
  }).fail(function () {
    $.ajax(host + endpointUnscheduledExams).fail(function(jqXHR, textStatus, errorThrown) {
      alert(jqXHR.responseText);
      $('#error').html(jqXHR.responseText);
    })
  });
};

let _fetchExamsData = function (inDay, inTime, slots) {
  for(var i in slots) {
    let slot = slots[i];
    let timeSlot = slot[0];
    let exams = slot[1];
    let day = timeSlot[0];
    let time = timeSlot[1];
    if(day == inDay && time == inTime) {
      let examsInSlot = exams.examsInSlot;
      var arr = [];
      for(var j in Object.keys(examsInSlot)) {
        let anCode = Object.keys(examsInSlot)[j];
        let exam = examsInSlot[anCode];
        if(anCode == null) {
          anCode = '';
        }
        let name = "";
        if(exam != null) {
          name = exam.name;
        }
        arr.push(anCode + `</br>` + name);
      }
      return arr;
    }
  }
};
let _getAncodesForSlot = function (inDay, inTime, slots) {
  for(var i in slots) {
    let slot = slots[i];
    let timeSlot = slot[0];
    let exams = slot[1];
    let day = timeSlot[0];
    let time = timeSlot[1];
    if(day == inDay && time == inTime) {
      let examsInSlot = exams.examsInSlot;
      var arr = [];

      for(var j in Object.keys(examsInSlot)) {
        let anCode = Object.keys(examsInSlot)[j];
        if(anCode == null) {
          anCode = '';
        }
        arr.push(anCode);
      }
      return arr;
    }
  }
};
let _fetchExamDescription = function (inDay, inTime, slots) {
  var description = 'text';

  return description;
};

let _fetchExamDays = function (waitTime, maxAttempts, currentAttemptNo) {
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
                          <th></th>`;
          for(let i in examDays) {
            let examDay = examDays[i];
            output += `<th>${examDay}</th>`;
          }
          output += `</tr>`;

          for(let i in slotsPerDay) {
            let slot = slotsPerDay[i];
            output += `<tr>
                          <td class="times">${slot}</td>`;
            for(let j in examDays) {
              let examDay = examDays[j];
              let examData = _fetchExamsData(j, i, slots);
              var anCodes = _getAncodesForSlot(j, i, slots);
              output += `<td class="exams">
                        <div class="outer" ondrop="dropExam(event)" ondragover="allowDropExam(event)">`
              for(let k in examData) {
                output += `<div id="${anCodes[k]}" class="inner" ondrop="return false;"
                            draggable="true" ondragstart="dragExam(event)"
                            onclick="viewDetails(${anCodes[k]})">${examData[k]}
                            </div>`;
              }
              output += `</div>
                        </td>`;
            }
            output += `</tr>`;
          }
          // Detailed description of the selected exams
          output += `</tr>
                </table>
                </td>
                  <td border="0" style="vertical-align:top; width: 200px; word-break: break-word">
                    <div id="description">
                    </div>
                  </td>
              </tr>
            </table>
            </br>`;
          $('#plan').html(output);
        });
      });
    })
    .fail(function () {
      $.ajax(host + endpointExamDays).fail(function(jqXHR, textStatus, errorThrown) {
        // alert(jqXHR.responseText);
        $('#error').append(jqXHR.responseText);
      })
    });
};

// Convenience function for _fetchExams
let fetchExams = function (waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchExams(waitTimeBetweenAttempt, maxNoOfAttempts, 1);
};

let fetchExamDays = function (waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchExamDays(waitTimeBetweenAttempt, maxNoOfAttempts, 1);
};

let fetchUnscheduledExams = function (waitTimeBetweenAttempt, maxNoOfAttempts){
  _fetchUnscheduledExams(waitTimeBetweenAttempt, maxNoOfAttempts, 1);
};
// Start trying to fetch the exam list
fetchExams(waitTimeBetweenAttempt, maxNoOfAttempts);

fetchExamDays(waitTimeBetweenAttempt, maxNoOfAttempts);

fetchUnscheduledExams(waitTimeBetweenAttempt, maxNoOfAttempts);
