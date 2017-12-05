function viewDetails (event, anCode) {
  var output = ''
  if (event.currentTarget.className.includes('div_select')) {
    $('#description').html('');
    $('.inner').on('click', function (event) {
      event.stopImmediatePropagation()
      toggleSelect($(this))
    })
    $('.innerUn').on('click', function () {
      event.stopImmediatePropagation()
      toggleSelect($(this))
    });

    $('#description').html('')
  } else {
    $('.inner').on('click', function (event) {
      event.stopImmediatePropagation()
      toggleSelect($(this))
    });
    $('.innerUnscheduled').on('click', function (event) {
      event.stopImmediatePropagation()
      toggleSelect($(this))
    })
    $.getJSON(host + endpointExams, function (exams) {
      var exam = null
      for (var i in exams) {
        if (exams[i].anCode === anCode) {
          exam = exams[i]
        }
      }
      let groupsOutput = groupsToHTML(exam.groups)
      let registeredGroupsOutput = registeredGroupsToHTML(exam.registeredGroups)
      output += ` <h2 >${exam.name}</h1></br>
                  AnCode: ${exam.anCode}</br>
                  Rooms: </br>
                  Groups: ${groupsOutput}
                  Registered Groups: ${registeredGroupsOutput}
                  PlannedByMe: ${exam.plannedByMe}</br>
                  StudentsWithHandicaps: </br>
                  Lecturer: <span class="lecturer">${exam.lecturer.personShortName}</span></br>
                  PersonFK: ${exam.lecturer.personFK}</br>
                  PersonIsLBA: <span class="lecturer">${exam.lecturer.personIsLBA}</span></br>
                  PersonID: ${exam.lecturer.personID}</br>
                  PersonEmail: ${exam.lecturer.personEmail}</br>
                  Duration: ${exam.duration}</br>
                  ReExam: ${exam.reExam} </br>
                  ExamType: ${exam.examType} </br>
                  Conflicting Ancodes:
                  <ul id='conflictingAncodes'>`
      for (let  i = 0; i < exam.conflictingAncodes.length; i++) {
        let conflictingAncode = exam.conflictingAncodes[i]
        output += `<li>${conflictingAncode}</li>`
      }
      output += `<ul>`
      output += `<br>Overlaps: <div id='overlaps'></div>`
      $('#description').html(output)
      fetchOverlaps(anCode)
      setConflicts(anCode, exam.conflictingAncodes)
      fetchExamsBySameLecturer(anCode)
    }).fail(function (jqXHR, textStatus, errorThrown) {
      $('#error').append(`Error on viewDetails: `)
      $('#error').append(jqXHR.responseText)
      $('#error').append(`<br>`)
      $('#error').css({
        'border': '3px solid #e22d2d'
      })
    })
  }
}

function toggleSelect (thisObj) {
  if (thisObj[0].className.includes('div_select')) {
    thisObj.parents('table').find('div').removeClass('div_select')
    thisObj.parents('div').find('div').removeClass('div_select')
    thisObj.parents('div').find('div').removeClass('overlap')
    thisObj.parents('div').find('div').removeClass('conflicts')
    thisObj.parents('div').find('div').removeClass('examsBySameLecturer')
  } else {
    thisObj.parents('table').find('div').removeClass('div_select')
    thisObj.parents('div').find('div').removeClass('div_select')
    thisObj.parents('div').find('div').removeClass('overlap')
    thisObj.parents('div').find('div').removeClass('conflicts')
    thisObj.parents('div').find('div').removeClass('examsBySameLecturer')
    thisObj.addClass('div_select')
  }
}

function fetchOverlaps (anCode) {
  var request = $.ajax({
    type: 'POST',
    url: host + endpointOverlaps,
    data: JSON.stringify(anCode),
    contentType: 'application/json',
    dataType: 'json'
  })
  request.done(function (overlappingExams) {
    selectOverlapExams(overlappingExams, anCode);
    var output = `
    <ul id='overlaps'>
    `
    for (var i = 0; i < overlappingExams.length; i++) {
      let group = overlappingExams[i]
      output +=
        `<li id='group'> ${group.olGroup.groupDegree}
          <div>`
      let overlap = group.olOverlaps[anCode]
      for (var name in overlap) {
        output += name + `: ` + overlap[name] + `</br>`
      }
      output += `
          </div>
          </li>`
    }
    output += `</ul>`
    $('#overlaps').html(output)
  })
}

function fetchExamsBySameLecturer (anCode) {
  var request = $.ajax({
    type: 'POST',
    url: host + endpointExamsBySameLecturer,
    data: JSON.stringify(anCode),
    contentType: 'application/json',
    dataType: 'json'
  })
  request.done(function (otherExams) {
    for (let i in otherExams) {
      let otherExam = otherExams[i]
      $('#'.concat(otherExam.anCode)).addClass('examsBySameLecturer')
    }
  })
}

function selectOverlapExams (overlaps, anCode) {
  for (var i = 0; i < overlaps.length; i++) {
    let group = overlaps[i]
    let overlap = group.olOverlaps[anCode]
    for (var name in overlap) {   
      $('#'.concat(name)).addClass('overlap')
    }
  }
}

function setConflicts (anCode, conflictingAncodes) {
  for (var i in conflictingAncodes) {
    let conflictingAncode = conflictingAncodes[i]
    $('#'.concat(conflictingAncode)).addClass('conflicts')
  }
}

function registeredGroupsToHTML (groups) {
  let output = `<ul id='registeredGroups'>`
  for (var i in groups) {
    let group = groups[i]
    output += `<li id='group'>
      ${group.registeredGroupDegree} (${group.registeredGroupStudents})
      </li>`
  }
  output += `</ul>`
  return output
}

function groupsToHTML (groups) {
  let output = `
  <ul id='groups'>
  `
  for (var i in groups) {
    let group = groups[i]
    let groupDegree = group.groupDegree != null ? group.groupDegree + ` ` : ``
    let groupSemester = group.groupSemester != null ? group.groupSemester + ` ` : ``
    let groupSubgroup = group.groupSubgroup != null ? group.groupSubgroup + ` ` : ``
    let groupRegistrations = group.groupRegistrations != null ? group.groupRegistrations + ` ` : ``
    output +=
      `<li id='group'>
        <div>` +
      groupDegree +
      groupSemester +
      groupSubgroup +
      groupRegistrations + `
        </div>
      </li>
      `
  }
  output += `</ul>`
  return output
}

function addExamToSlot (anCode, dayIdx, slotIdx) {
  var result = false
  $.ajax({
    type: 'POST',
    url: host + endpointAddExam,
    data: JSON.stringify({
      planManipAnCode: anCode,
      planManipDay: dayIdx,
      planManipSlot: slotIdx
    }),
    success: function (data) {
      if (data.tag !== 'Ok') {
        alert(data.contents)
      } else {
        fetchExamDays()
        fetchUnscheduledExams()
        result = true
      }
    },
    contentType: 'application/json',
    dataType: 'json'
  })
  return result
}

function dropExam (ev) {
  var data = parseInt(ev.dataTransfer.getData('text'))
  var day = parseInt(ev.currentTarget.getAttribute('data-day'))
  var slot = parseInt(ev.currentTarget.getAttribute('data-slot'))
  let dropped = addExamToSlot(data, day, slot)
  if (dropped) {
    ev.currentTarget.appendChild(document.getElementById(data))
    if (ev.currentTarget.className === 'outer') {
      document.getElementById(data).className = 'inner'
    } else if (ev.currentTarget.className === 'outerUnscheduled') {
      document.getElementById(data).className = 'innerUnscheduled'
    }
    _fetchValidation()
  }
}

function dragExam (ev) {
  ev.dataTransfer.setData('text', ev.target.id)
}

function allowDropExam (ev) {
  if ((ev.currentTarget.className === 'outer') || (ev.currentTarget.className === 'outerUnscheduled')) {
    ev.preventDefault()
  }
}
