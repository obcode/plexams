const fetchRooms = () => {
  $.getJSON(endpoints.semesterConfig, function (semesterConfig) {
    $.getJSON(endpoints.examDays, function (examDays) {
        $.getJSON(endpoints.slotsPerDay, function (slotsPerDay) {
          $.getJSON(endpoints.slots, function (slots) {
            // Construct the plan output
            const draggable = !semesterConfig.scheduleFrozen
            let output =
              `<table>
                      <tr>
                      <td>
                        <table>
                          <tr>
                            <th></th>`
            for (let i in examDays) {
              let examDay = examDays[i]
              output += `<th class="days">(${i}) ${examDay}</th>`
            }
            output += `</tr>`

            for (let i in slotsPerDay) {
              let slot = slotsPerDay[i]
              output += `<tr>
                            <td class="times">${slot} (${i})</td>`
              for (let j in examDays) {
                let examDay = examDays[j]
                let examData = _fetchExamsData(j, i, slots)
                var anCodes = _getAncodesForSlot(j, i, slots);
                output += `<td class="rooms">
                          <div id="slot_${j}_${i}" class="outer" data-day="${j}" data-slot="${i}">`
                for (let k in examData) {
                  const exam = examData[k]
                  for (let r in exam.rooms) {
                    const room = exam.rooms[r]
                    output += `<div id="${exam.anCode}-${room.roomID}"
                              onclick="openModal('modal-plannedRooms-${room.roomID}')"
                              class="room `
                    if (room.handicapCompensation) {
                      output += 'NTA'
                    }
                    if (room.reserveRoom) {
                      output += 'Reserve'
                    }
                    if (room.roomID === 'R1.046') {
                      output += 'room-R1.046'
                    }
                    if (room.roomID === 'R1.049') {
                      output += 'room-R1.049'
                    }
                    output += `"
                               onclick="viewDetails(event, ${exam.anCode})"
                               onmouseover="">${room.roomID} (${room.studentsInRoom.length}/${room.maxSeats})</div>`
                  }
                }
                output += `</div>
                          </td>`
              }
              output += `</tr>`
            }
            // Detailed description of the selected exams
            output += `</tr>
                  </table>
                  </td>
                </tr>
              </table>
              <div id="roomsModal"></div>`
            $('#rooms').html(output)
            $.getJSON(endpoints.plannedRooms, function (plannedRooms) {
              let output = ''
              for (let i in plannedRooms) {
                const plannedRoom = plannedRooms[i]
                output += `<div id="modal-plannedRooms-${plannedRoom.plannedRoomID}" class="modal">
                          <div class="modal-content">
                          <span id="modal-plannedRooms-${plannedRoom.plannedRoomID}-close">&times;</span>
                          <h2>${plannedRoom.plannedRoomID}</h2><ul class="roomSlotList">`
                for (let d in plannedRoom.plannedRoomDaysAndSlots) {
                  const day = plannedRoom.plannedRoomDaysAndSlots[d]
                  output += `<li>${day.plannedRoomDay}
                              <ul class="roomSlotList">`
                  for (let s in day.plannedRoomSlots) {
                    const slot = day.plannedRoomSlots[s]
                    output += `<li>${slot}</li>`
                  }
                  output += `</ul></li>`
                }
                output += `<ul></div></div>`
              }
              $('#roomsModal').html(output)
            })
          })
        })
    })
    .fail(function (jqXHR, textStatus, errorThrown) {
      $('#error').append(`Error on endpoint \\examDays: `)
      $('#error').append(jqXHR.responseText)
      $('#error').append(`<br>`)
      $('#error').css({
        'border': '3px solid #e22d2d'
      })
    })
  })
}
