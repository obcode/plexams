// Backend and endpoint details
const host             = 'http://127.0.0.1:8080'
const endpointExams    = '/exams'
const endpointExamDays = '/examDays'
// Retry configuration
let maxNoOfAttempts        = 50,
    waitTimeBetweenAttempt = 250

let _fetchExams = function(waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + endpointExams, function(exams) {
    $('#status').html(`Fetched the content after attemt no.
                       ${currentAttemptNo}!`)
    // Construct the user list HTML output
    let output =
      `<table>
        <tr>
          <th>Prüfung</th>
          <th>Prüfer</th>
          <th>Anmeldecode</th>
          <th>Wiederholungsklausur</th>
       </tr>`;
    for (let i in exams) {
      let exam = exams[i]
      output += `<tr>
               <td>${exam.name}</td>
               <td>${exam.lecturer.personShortName}</td>
               <td>${exam.anCode}</td>
               <td>${exam.reExam}</td>
               </tr>`
    }
    output += `</table>`
    $('#plexams-api').html(output)
  }).fail(function() {
    $('#status').html(`Attempt no. <b>${currentAttemptNo}</b>. Are you sure the
                       server is running on <b>${host}</b>, and the endpoint
                       <b>${endpointExams}</b> is correct?`)
    // Keep trying until we get an answer or reach the maximum number of retries
    if (currentAttemptNo < maxAttempts) {
      setTimeout(function() {
        _fetchExams(waitTime, maxAttempts, currentAttemptNo + 1)
      }, waitTime)
    }
  })
}

let _fetchExamDays = function (waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + endpointExamDays, function (examDays) {
      // Construct the plan output

      let output = `<table>
                      <tr>`;
      for (let i in examDays) {
        let examDay = examDays[i]
        output += `<th>${examDay}</th>`
      }
      output += `</tr>`

      // for (let i in exams) {
      //   let exam = exams[i]
      //   output += `<tr>
      //              <td>${exam.name}</td>
      //              <td>${exam.lecturer.personShortName}</td>
      //              <td>${exam.anCode}</td>
      //              <td>${exam.reExam}</td>
      //              </tr>`
      // }
      output += `</table>`
      $('#plan').html(output)
    })
    .fail(function () {
      $('#status')
        .html(`Attempt no. <b>${currentAttemptNo}</b>. Are you sure the
                       server is running on <b>${host}</b>, and the endpoint
                       <b>${endpointExamDays}</b> is correct?`)
      // Keep trying until we get an answer or reach the maximum number of retries
      if (currentAttemptNo < maxAttempts) {
        setTimeout(function () {
          _fetchExamDays(waitTime, maxAttempts, currentAttemptNo + 1)
        }, waitTime)
      }
    })
}

// Convenience function for _fetchExams
let fetchExams = function (waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchExams(waitTimeBetweenAttempt, maxNoOfAttempts, 1)
}

let fetchExamDays = function (waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchExamDays(waitTimeBetweenAttempt, maxNoOfAttempts, 1)
}

// Start trying to fetch the exam list
fetchExams(waitTimeBetweenAttempt, maxNoOfAttempts)

fetchExamDays(waitTimeBetweenAttempt, maxNoOfAttempts)
