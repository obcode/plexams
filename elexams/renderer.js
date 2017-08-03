// Backend and endpoint details
const host     = 'http://127.0.0.1:8080'
const endpoint = '/exams'
// Retry configuration
let maxNoOfAttempts        = 50,
    waitTimeBetweenAttempt = 250

let _fetchUserList = function(waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + endpoint, function(exams) {
    $('#status').html(`Fetched the content after attemt no.
                       ${currentAttemptNo}!`)
    // Construct the user list HTML output
    let output =
      `<table>
        <tr>
          <th>Prüfung</th>
          <th>Prüfer</th>
          <th>Datum, Zeit</th>
          <th>Anmeldungen</th>
       </tr>`;
    for (let i in exams) {
      let exam = exams[i]
      output += `<tr><td>${exam.name}</td>
                 <td>${exam.lecturer}</td>
                 <td>${exam.datetime}</td>
                 <td>${exam.registrations}</td>
                 </tr>`
    }
    output += `</table>`
    $('#plexams-api').html(output)
  }).fail(function() {
    $('#status').html(`Attempt no. <b>${currentAttemptNo}</b>. Are you sure the
                       server is running on <b>${host}</b>, and the endpoint
                       <b>${endpoint}</b> is correct?`)
    // Keep trying until we get an answer or reach the maximum number of retries
    if (currentAttemptNo < maxAttempts) {
      setTimeout(function() {
        _fetchUserList(waitTime, maxAttempts, currentAttemptNo+1)
      }, waitTime)
    }
  })
}

// Convenience function for _fetchUserList
let fetchUserList = function(waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchUserList(waitTimeBetweenAttempt, maxNoOfAttempts, 1)
}

// Start trying to fetch the user list
fetchUserList(waitTimeBetweenAttempt, maxNoOfAttempts)
