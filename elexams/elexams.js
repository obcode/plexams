// const host = 'http://127.0.0.1:8080';
// const endpointSlots = '/slots';
const endpointAddExam = '/addExam';
const slotsPerDay = 6;

function viewDetails(anCode) {
  $.getJSON(host + endpointExams, function (exams) {
    $(".inner").on("click", function () {
      toggleSelect($(this));
    });
    $(".innerUn").on("click", function () {
      toggleSelect($(this));
    });
    var output = "";
    var exam = null;
    for(var i in exams) {
      if(exams[i].anCode == anCode) {
        exam = exams[i];
      }
    }
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
  });
  // .fail(function () {});
}

function toggleSelect(thisObj) {
  thisObj.parents("table").find('div').removeClass("div_select");
  thisObj.parents("div").find('div').removeClass("div_select");
  thisObj.toggleClass("div_select");
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
      </li>
      `;
  }
  output += `</ul>`;
  return output;
}

function addExamToSlot(anCode, dayIdx, slotIdx) {
  // if(confirm('Are you sure you want to move this exam?')) {
  // Save it!
  var result = false;
  $.ajax({
    type: 'POST',
    url: host + endpointAddExam,
    data: JSON.stringify({
      planManipAnCode: anCode,
      planManipDay: dayIdx,
      planManipSlot: slotIdx
    }),
    success: function (data) {
      if(data.tag != "Ok") {
        alert(data.contents);
      } else {
        fetchExamDays(100, 100);
        result = true;
      }
    },
    contentType: "application/json",
    dataType: 'json'
  });
  return result;
}

function dropExam(ev) {
  var data = parseInt(ev.dataTransfer.getData("text"));
  var day = parseInt(ev.currentTarget.getAttribute("data-day"));
  var slot = parseInt(ev.currentTarget.getAttribute("data-slot"));
  let dropped = addExamToSlot(data, day, slot)
  if(dropped) {
    ev.currentTarget.appendChild(document.getElementById(data));
    if(ev.currentTarget.className == "outer") {
      document.getElementById(data).className = "inner";
    } else if(ev.currentTarget.className == "outerUnscheduled") {
      document.getElementById(data).className = "innerUnscheduled";
    }
  }
}

function dragExam(ev) {
  ev.dataTransfer.setData("text", ev.target.id);
}

function allowDropExam(ev) {
  if((ev.currentTarget.className == "outer") || (ev.currentTarget.className == "outerUnscheduled")) {
    ev.preventDefault();
  }
}
