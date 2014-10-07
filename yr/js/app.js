// DOMContentLoaded is fired once the document has been loaded and parsed,
// but without waiting for other external resources to load (css/images/etc)
// That makes the app more responsive and perceived as faster.
// https://developer.mozilla.org/Web/Reference/Events/DOMContentLoaded
window.addEventListener('DOMContentLoaded', function() {

  // We'll ask the browser to use strict code to help us catch errors earlier.
  // https://developer.mozilla.org/Web/JavaScript/Reference/Functions_and_function_scope/Strict_mode
  'use strict';

  var apiURL = 'https://developer.mozilla.org/search.json?q=';
  var errorMsg = document.getElementById('error');
  var searchInput = document.getElementById('term');
  var results = document.getElementById('results');
  var request = null;
  var translate = navigator.mozL10n.get;
  var form = document.querySelector('form');

  // Forms will take the values in the input fields they contain
  // and send them to a server for further processing,
  // but since we want to stay in this page AND make a request to another server,
  // we will listen to the 'submit' event, and prevent the form from doing what
  // it would usually do, using preventDefault.
  // Read more about it here:
  // https://developer.mozilla.org/Web/API/event.preventDefault
  //
  // Then we search without leaving this page, just as we wanted.
  form.addEventListener('submit', function(e) {
      e.preventDefault();
      search();
  }, false);

  // We want to wait until the localisations library has loaded all the strings.
  // So we'll tell it to let us know once it's ready.
  navigator.mozL10n.once(search);

  // ---

  function search() {

    // Are we searching already? Then stop that search
    if(request && request.abort) {
      request.abort();
    }


    results.textContent = translate('searching');

    // We will be using the 'hidden' attribute throughout the app rather than a
    // 'hidden' CSS class because it enhances accessibility.
    // See: http://www.whatwg.org/specs/web-apps/current-work/multipage/editing.html#the-hidden-attribute
    results.hidden = false;
    errorMsg.hidden = true;


    var term = searchInput.value;
    if(term.length === 0) {
      term = searchInput.placeholder;
    }
    var url = "http://www.yr.no/stad/Noreg/Telemark/Sauherad/Gvarv/varsel.xml";
    var url = "http://www.yr.no/sted/Norge/%C3%98stfold/Askim/Askim/varsel.xml";
    
//    var url = apiURL + term;

    // If you don't set the mozSystem option, you'll get CORS errors (Cross Origin Resource Sharing)
    // You can read more about CORS here: https://developer.mozilla.org/docs/HTTP/Access_control_CORS
    request = new XMLHttpRequest({ mozSystem: true });

    request.open('get', url, true);
    //request.responseType = 'xml';

    // We're setting some handlers here for dealing with both error and
    // data received. We could just declare the functions here, but they are in
    // separate functions so that search() is shorter, and more readable.
    request.addEventListener('error', onRequestError);
    request.addEventListener('load', onRequestLoad);

    request.send();

  }

  function onRequestError() {

    var errorMessage = request.error;
      if(!errorMessage) {
        errorMessage = translate('searching_error');
      }
      showError(errorMessage);

  }

  // Features we don't have:
  //  - favorites storage - will store the lookup keys at a minimum, may cache a forecast too
  //  - nice symbols for the summary
  //  - ability to look up new places and pick one
  //  - display a sheet of favorites, with details on second sheet
  //  - language selection?
  
  function get(e, ...ns) {
    for ( var i=0 ; i < ns.length ; i++ )
      e = e.getElementsByTagName(ns[i])[0];
    return e;
  }
  
  function fixup(s) {
    if (s == "Delvis skyet") return "D.skyet";
    return s;
  }
  function onRequestLoad() {

    var response = request.responseXML;
    if(response === null) {
      showError(translate('searching_error'));
      return;
    }
    
    var name = get(response, "weatherdata", "location", "name").textContent;
    var times = get(response, "weatherdata", "forecast", "tabular").getElementsByTagName("time");
    var forecast = [];
    var timestamp = /(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)/; // 1=year 2=month 3=day 4=hour 5=min 6=sec
    for ( var i=0 ; i < times.length ; i++ )
      forecast.push({temp: get(times[i], "temperature").getAttribute("value"), 
                     summary: get(times[i], "symbol").getAttribute("name"),
                     precip: get(times[i], "precipitation").getAttribute("value"),
                     from: times[i].getAttribute("from").match(timestamp),
                     to: times[i].getAttribute("to").match(timestamp)});
    var computedResponse =
        "<div><table><tbody>" +
        "<tr><td><b>" + name + "</b></td><td></td><td></td><td></td></tr>\n" + 
        "<tr><td><b>Tid</b></td><td><b>Temp</b></td><td><b>Vaer</b></td><td><b>Nedboer</b></td></tr>\n" + 
        forecast.map((x) => "<tr>" +
                            "<td>" + x.from[4] + "-" + x.to[4] + "</td>" +
                            "<td>" + x.temp + "</td>" +
                            "<td>" + fixup(x.summary) + "</td>" +
                            "<td>" + x.precip +  "</td></tr>\n").join("") + 
        "</tbody></table></div>";
    
    results.innerHTML = computedResponse;
    results.hidden = false;
    return;
    
    var response = request.response;

    if(response === null) {
      showError(translate('searching_error'));
      return;
    }

    results.textContent = '';

    var documents = response.documents;

    if(documents.length === 0) {

      var p = document.createElement('p');
      p.textContent = translate('search_no_results');
      results.appendChild(p);

    } else {

      documents.forEach(function(doc) {

        // We're using textContent because inserting content from external sources into your page using innerHTML can be dangerous.
        // https://developer.mozilla.org/Web/API/Element.innerHTML#Security_considerations
        var docLink = document.createElement('a');
        docLink.textContent = doc.title;
        docLink.href = doc.url;

        // We want the links to open in a pop up window with a 'close'
        // button, so that the user can consult the result and then close it and
        // be brought back to our app.
        // If we did nothing, these external links would take over the entirety
        // our app and there would be no way for a user to go back to the app.
        // But Firefox OS allows us to open ONE new window per app; these new
        // windows will have a close button, so the user can close the overlay
        // when they're happy with what they've read.
        // Therefore we will capture click events on links, stop them from
        // doing their usual thing using preventDefault(),
        // and then open the link but in a new window.
        docLink.addEventListener('click', function(evt) {
          evt.preventDefault();
          window.open(evt.target.href, 'overlay');
        });

        var h2 = document.createElement('h2');
        h2.appendChild(docLink);
        results.appendChild(h2);

      });

    }

    // And once we have all the content in place, we can show it.
    results.hidden = false;

  }


  function showError(text) {
    errorMsg.textContent = text;
    errorMsg.hidden = false;
    results.hidden = true;
  }

});
