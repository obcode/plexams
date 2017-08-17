// const host = 'http://127.0.0.1:8080';
// const endpointSlots = '/slots';
const endpointAddExam = '/addExam';
const slotsPerDay = 6;
var _anCode = 0;

function viewDetails(day, time, x) {
  $.getJSON(host + endpointSlots, function (slots) {
    $(".table").on("click", "td", function () {
      $(this).parents("table").find('td').removeClass("td_select");
      $(this).toggleClass("td_select");
    });
    var slot = slots[(slotsPerDay * day) + time];
    var exams = slot[1];
    var output = "";
    let examsInSlot = exams.examsInSlot;
    let exam = examsInSlot[Object.keys(examsInSlot)[x]];
    let groupsOutput = groupsToHTML(exam.groups);
    output += ` <h2 >${exam.name}</h1></br>
                AnCode: ${exam.anCode}</br>
                Rooms: </br>
                Groups: ${groupsOutput}
                PlannedByMe: ${exam.plannedByMe}</br>
                StudentsWithHandicaps: </br>
                Lecturer: ${exam.lecturer.personShortName}</br>
                PersonFK: ${exam.lecturer.personFK}</br>
                PersonIsLBA: ${exam.lecturer.personIsLBA}</br>
                PersonID: ${exam.lecturer.personID}</br>
                PersonEmail: ${exam.lecturer.personEmail}</br>
                Duration: ${exam.duration}</br>
                ReExam: ${exam.reExam} </br>
                ExamType: ${exam.examType}`;
    $('#description').html(output);
    _anCode = exam.anCode;
  });
  // .fail(function () {});
}

function groupsToHTML(groups) {
  output = `
  <ul id="groups">
  `;
  for(var i in groups) {
    let group = groups[i];
    groupDegree = group.groupDegree != null ? group.groupDegree + ` ` : ``;
    groupSemester = group.groupSemester != null ? group.groupSemester + ` ` : ``;
    groupSubgroup = group.groupSubgroup != null ? group.groupSubgroup + ` ` : ``;
    groupRegistrations = group.groupRegistrations != null ? group.groupRegistrations + ` ` : ``;
    output +=
      `<li id="group">
        <div>` +
      groupDegree +
      groupSemester +
      groupSubgroup +
      groupRegistrations + `
        </div>
      </<li>
      `;
  }
  output += `</ul>`;
  return output;
}

function addExamToSlot(dayIdx, slotIdx) {
  if(confirm('Are you sure you want to save this thing into the database?')) {
    // Save it!
    $.ajax({
      type: 'POST',
      url: host + endpointAddExam,
      data: JSON.stringify({
        anCode1: _anCode,
        day: dayIdx,
        slot1: slotIdx
      }),
      success: function (data) {},
      contentType: "application/json",
      dataType: 'json'
    });
    fetchExamDays(100, 100);
  } else {
    // Do nothing!
  }
}
