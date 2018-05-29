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
    $.getJSON(endpoints.exams, function (exams) {
      var exam = null
      for (var i in exams) {
        if (exams[i].anCode === anCode) {
          exam = exams[i]
        }
      }
      output += ` <h2 >${exam.anCode}. ${exam.name}</h2>
                  ${exam.duration} Minuten`
      if (exam.reExam) {
        output += ', Wiederholungsprüfung'
      } else {
        output += ', Erstprüfung'
      }
      output += ` <br>
                  <a href="mailto:${exam.lecturer.personEmail}"> ${exam.lecturer.personID}.
                  <span class="lecturer">${exam.lecturer.personShortName}</span>`
      if (exam.lecturer.personFK !== 'FK07' && exam.lecturer.personFK !== '') {
        output += `, ${exam.lecturer.personFK}`
      }
      if (exam.lecturer.personIsLBA) {
        output += ', LBA'
      }
      output += ` </a></br>`
      output += groupsToHTML(exam.groups)
      output += registeredGroupsToHTML(exam.registeredGroups)
      output += `<ul class="rooms">`
      for (let i in exam.rooms) {
        const room = exam.rooms[i]
        output += `<li class="room ${room.roomID}">${room.roomID}: `
        output += `${room.studentsInRoom.length}/${room.maxSeats}`
        if (room.reserveRoom) {
          output += ' (Reserve)'
        }
        if (room.handicapCompensation) {
          output += ' (NTA)'
        }
        output += `</li>`
      }
      output += '</ul>'
      if (exam.handicapStudents.length > 0) {
        output += '<span class="NTA">Nachteilsausgleich</span><ul class="handicapStudents">'
        for (let h in exam.handicapStudents) {
          const student = exam.handicapStudents[h]
          output +=
            `<li class="handicapStudents">
            ${student.studentName}<br>
            ${student.studentHandicap.handicapCompensationText}
             </li>`
        }
      }
      output += '</ul>'
      output += Object.keys(exam.conflictingAncodes).length + ' Konflikte: '
      Object.keys(exam.conflictingAncodes).forEach((k) => {
        output += `${k} (${exam.conflictingAncodes[k]}),`
      })
      $('#description').html(output)
      setConflicts(anCode, exam.conflictingAncodes)
      setConflictingSlots(anCode)
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
  thisObj.parents('table').find('div').removeClass('div_select')
  thisObj.parents('div').find('div').removeClass('div_select')
  thisObj.parents('div').find('div').removeClass('overlap')
  thisObj.parents('div').find('div').removeClass('conflictsLT5')
  thisObj.parents('div').find('div').removeClass('conflictsLT10')
  thisObj.parents('div').find('div').removeClass('conflictsLT20')
  thisObj.parents('div').find('div').removeClass('conflicts')
  thisObj.parents('div').find('div').removeClass('conflictingSlot')
  thisObj.parents('div').find('div').removeClass('examsBySameLecturer')
  if (!thisObj[0].className.includes('div_select')) {
    thisObj.addClass('div_select')
  }
}

function fetchExamsBySameLecturer (anCode) {
  var request = $.ajax({
    type: 'POST',
    url: endpoints.examsBySameLecturer,
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

function setConflicts (anCode, conflicting) {
  const conflictingAncodes = Object.keys(conflicting)
  for (var i in conflictingAncodes) {
    let conflictingAncode = conflictingAncodes[i]
    if (conflicting[conflictingAncode] < 5) {
      $('#'.concat(conflictingAncode)).addClass('conflictsLT5')
    } else if (conflicting[conflictingAncode] < 10) {
      $('#'.concat(conflictingAncode)).addClass('conflictsLT10')
    } else if (conflicting[conflictingAncode] < 20) {
      $('#'.concat(conflictingAncode)).addClass('conflictsLT20')
    } else {
      $('#'.concat(conflictingAncode)).addClass('conflicts')
    }
  }

}

function setConflictingSlots (anCode) {
  var request = $.ajax({
    type: 'POST',
    url: endpoints.conflictingSlots,
    data: JSON.stringify(anCode),
    contentType: 'application/json',
    dataType: 'json'
  })
  request.done(function (slots) {
    for (let i in slots) {
      const slot = slots[i]
      $(['#slot_', slot[0], '_', slot[1]].join('')).addClass('conflictingSlot')
    }
  })
}

function groupsToHTML (groups) {
  let output = `<div class='groups'>Angeboten für:`
  for (var i in groups) {
    let group = groups[i]
    output += `<span class='group'>
      ${group.groupDegree}`
    if (group.groupSemester !== null) {
      output += group.groupSemester
    }
    if (group.groupSubgroup !== null) {
      output += group.groupSubgroup
    }
    output += '</span>'
    if (i < groups.length - 1) {
      output += ','
    }
  }
  output += `</div>`
  return output
}

function registeredGroupsToHTML (groups) {
  let output = `<div class='registeredGroups'>`
  for (var i in groups) {
    let group = groups[i]
    output += `<span class='group'>
      ${group.registeredGroupDegree} (${group.registeredGroupStudents})
      </span>`
    if (i < groups.length - 1) {
      output += ','
    }
  }
  output += `</div>`
  return output
}

function addExamToSlot (anCode, dayIdx, slotIdx) {
  var result = false
  $.ajax({
    type: 'POST',
    url: endpoints.addExam,
    data: JSON.stringify({
      planManipAnCode: anCode,
      planManipDay: dayIdx,
      planManipSlot: slotIdx
    }),
    success: function () {
      fetchExamDays()
      fetchUnscheduledExams()
      result = true
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
  if ((ev.currentTarget.classList.contains('outer')) || (ev.currentTarget.className === 'outerUnscheduled')) {
    ev.preventDefault()
  }
}

const fetchNTA = () => {
  $.getJSON(endpoints.examsWithNTA, (examsWithNTA) => {
    let output = `<h1>Nachteilsausgleich</h1>
                  <ol class="Nachteilsausgleich">`
    for (let i in examsWithNTA) {
      const exam = examsWithNTA[i]
      output += `<li class="Nachteilsausgleich">${exam.lecturer.personFullName}
        <a href="mailto:${exam.lecturer.personEmail}">&lt;${exam.lecturer.personEmail}&gt;</a><br>
        ${exam.anCode}. ${exam.name}
        <ul class="Nachteilsausgleich">`
      for (let j in exam.handicapStudents) {
        const student = exam.handicapStudents[j]
        output += `<li class="handicapStudents">
                    ${student.studentFamilyname}, ${student.studentFirstname}: ${student.studentHandicap.handicapCompensationText}
                   </li>`
      }
      output += '</ul></li>'
    }
    output += '</ul>'
    $('#NTA').html(output)
  })
}
