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
                ExamType: ${exam.examType} </br>
                Overlaps: <div id="overlaps"></div>`;
    $('#description').html(output);
    fetchOverlaps(anCode);
  }).fail(function (jqXHR, textStatus, errorThrown) {
    $('#error').append(`Error on viewDetails: `);
    $('#error').append(jqXHR.responseText);
    $('#error').append(`<br>`);
    $('#error').css({
      'border': "3px solid #e22d2d"
    });
  });
}

function toggleSelect(thisObj) {
  thisObj.parents("table").find('div').removeClass("div_select");
  thisObj.parents("div").find('div').removeClass("div_select");
  thisObj.toggleClass("div_select");
}

function fetchOverlaps(anCode) {
  var request = $.ajax({
    type: 'POST',
    url: host + endpointOverlaps,
    data: JSON.stringify(anCode),
    contentType: "application/json",
    dataType: 'json'
  });
  request.done(function (data) {
    var output = `
    <ul id="overlaps">
    `;
    for(var i = 0; i < data.length; i++) {
      let group = data[i];
      output +=
        `<li id="group"> ${group.olGroup.groupDegree}
          <div>`;
      let overlap = group.olOverlaps[anCode];
      for(var name in overlap) {   
        output += name + `: ` + overlap[name] + `</br>`;    
      }
      output += `
          </div>
          </li>`;
    }
    output += `</ul>`;
    $('#overlaps').html(output);
  });
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
        fetchExamDays();
        fetchUnscheduledExams();
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
