let fetchValidation = () => {
  $.getJSON(endpoints.validateWhat, (validateWhats) => {
    let output = '<div class="validationtab">'
    for (let i in validateWhats) {
      const validateWhat = validateWhats[i]
      output += `<button class="validationtablinks"
                  onclick="openValidation(event, '${validateWhat}')">${validateWhat}</button>`
    }
    output += '</div>'
    for (let i in validateWhats) {
      const validateWhat = validateWhats[i]
      output += `<div id="validation${validateWhat}" class="validatetabcontent">
        <div id="validation${validateWhat}"></div>
      </div>`
    }
    $('#validation').html(output)
  })
}

const openValidation = (evt, validateWhat) => {
  // Get all elements with class="tabcontent" and hide them
  const tabcontent = document.getElementsByClassName('validatetabcontent')
  for (let i = 0; i < tabcontent.length; i++) {
      tabcontent[i].style.display = 'none'
  }

  // Get all elements with class="tablinks" and remove the class "active"
  const tablinks = document.getElementsByClassName('validationtablinks');
  for (let i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(' active', '')
  }

  // Show the current tab, and add an "active" class to the button that opened the tab
  document.getElementById('validation' + validateWhat).style.display = 'block'
  evt.currentTarget.className += ' active'

  $.ajax({
    type: 'POST',
    url: endpoints.validation,
    data: JSON.stringify([validateWhat]),
    success: (validation) => {
      let output = `<h1>${validation.result}</h1><ul>`
      for (let i in validation.brokenConstraints) {
        let constraint = validation.brokenConstraints[i]
        output += `<li><span class="${constraint.tag}">
                    ${constraint.contents}</span>
                  </li>`
      }
      output += `</ul>`
      $('#validation'+validateWhat).html(output)
    },
    contentType: 'application/json',
    dataType: 'json'
  })
}








let _fetchValidation = () => {
  $.getJSON(endpoints.validation, (validation) => {
    let output = `<h1>${validation.result}</h1><ul>`
    for (let i in validation.brokenConstraints) {
      let constraint = validation.brokenConstraints[i]
      if (constraint.tag === 'HardConstraintBroken') {
        output += `<li><span class="${constraint.tag}">
                    ${constraint.contents}</span>
                  </li>`
      }
    }
    output += `</ul>`
    $('#validation').html(output)

    output = `<h1>${validation.result}</h1><ul>`
    for (let i in validation.brokenConstraints) {
      let constraint = validation.brokenConstraints[i]
      output += `<li><span class="${constraint.tag}">
                  ${constraint.contents}</span>
                </li>`
    }
    output += `</ul>`
    $('#validation-full').html(output)

  })
}

let fetchValidationSchedule = () => {
  var request = $.ajax({
    type: 'POST',
    url: endpoints.validation,
    data: JSON.stringify(["ValidateSchedule"]),
    contentType: 'application/json',
    dataType: 'json'
  })
  request.done(function (validation) {
    output = `<h1>${validation.result}</h1><ul>`
    for (let i in validation.brokenConstraints) {
      let constraint = validation.brokenConstraints[i]
      output += `<li><span class="${constraint.tag}">
                  ${constraint.contents}</span>
                </li>`
    }
    output += `</ul>`
    $('#validation-full').html(output)
  })
}
