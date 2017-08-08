const host = 'http://127.0.0.1:8080';
const endpointSlots = '/slots';
const slotsPerDay = 6;

function myFunction(day, time, x) {
  $.getJSON(host + endpointSlots, function (slots) {
    var slot = slots[(slotsPerDay * day) + time];
    var exams = slot[1];
    var output = "";
    let examsInSlot = exams.examsInSlot;
    let exam = examsInSlot[Object.keys(examsInSlot)[x]];
    output += ` AnCode: ${exam.anCode}</br>
                Name: ${exam.name}</br>
                Rooms: </br>
                Groups: </br>
                Slot: </br>
                PlannedByMe: ${exam.plannedByMe}</br>
                StudentsWithHandicaps: </br>
                Lecturer: ${exam.lecturer.personShortName}</br>
                PersonFK: ${exam.lecturer.personFK}</br>
                PersonIsLBA: ${exam.lecturer.personIsLBA}</br>
                PersonID: ${exam.lecturer.personID}</br>
                PersonEmail: ${exam.lecturer.personEmail}</br>
                Duration: ${exam.duration}</br>
                ReExam: false: </br>
                ExamType: sp60`;
    $('#description').html(output);
  });
  // .fail(function () {});
}
