// Backend and endpoint details
const host = 'http://127.0.0.1:8080';
const endpointExams = '/exams';
const endpointExamDays = '/examDays';
const endpointSlots = '/slots';
const endpointSlotsPerDay = '/slotsPerDay';

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

let _fetchExamsData = function (inDay, inTime, slots) {
  for(var i in slots) {
    let slot = slots[i];
    let timeSlot = slot[0];
    let exams = slot[1];
    let day = timeSlot[0];
    let time = timeSlot[1];
    if(day == inDay && time == inTime) {
      let examsInSlot = exams.examsInSlot;
      let reserveInvigilator = exams.reserveInvigilator;
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
                          <td>${slot}</td>`;
            for(let j in examDays) {
              let examDay = examDays[j];
              let examData = _fetchExamsData(j, i, slots);
              output += `<td>
                            <table id="inner" class="table">
                              <tr id="inner">`;
              for(let k in examData) {
                output += `<td id="inner" onclick="viewDetails(${j}, ${i}, ${k})">${examData[k]}</td>`;
              }
              output += `</tr>
                        </table id="inner">
                      </td>`;
            }
            output += `</tr>`;
          }
          output += `</tr>
                </table>
                </td>
                  <td border="0" style="padding:0; vertical-align:top; height: 100%; width: 20%;">
                    <table style="width:100%; height: 100%">
                      <tr style="min-height: 50%;">
                        <td border="0" style="vertical-align:top; height: 100%; width: 100%;">
                          <div id="inner" height="100%" style="border: 0; padding: 5px;">
                            <textbox id="description" >
                            </textbox>
                          </div>
                        </td>
                      </tr>
                      <tr style="min-height: 50%;">
                        <td style="padding:0; vertical-align:bottom; height: 100%">
                          <table style="padding:5px; height: 100%; width: 100%">`;
          for(let i in slotsPerDay) {
            output += `<tr>`;
            for(let j in examDays) {
              output += `<td onclick="addExamToSlot(${j},${i})" style="padding: 3px;"> ${j},${i} </td>`;
            }
            output += `</tr>`;
          }
          output += `</table>
                        </td>
                      </tr>
                    </table>
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

// Start trying to fetch the exam list
fetchExams(waitTimeBetweenAttempt, maxNoOfAttempts);

fetchExamDays(waitTimeBetweenAttempt, maxNoOfAttempts);
