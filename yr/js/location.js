/* -*- mode: java; c-basic-offset: 2; indent-tabs-mode: nil; tab-width: 2 -*- */

const en_countries = { FI: "Finland", NO: "Norway", SE: "Sweden" };
const no_countries = { FI: "Finland", SE: "Sverige" };

// Quasi-constants: HTML elements
var errorMsg;
var results;
var settingsElt;
var settingsButton;

// Globals
var setting_display_language;
var setting_forecast_language;
var setting_locations;
var setting_index;

window.addEventListener('DOMContentLoaded', LocationLogic);

function LocationLogic() {  
  results = document.getElementById('results');
  leftButton = document.getElementById('left_button');
  rightButton = document.getElementById('right_button');
  LoadDatabase();
  leftButton.onclick = GoLeft;
  rightButton.onclick = GoRight;
  LoadWeather();
}

function GoLeft() {
  if (setting_index == 0)
    window.location.replace("/settings.html");
  else {
    sessionStorage.setItem("location", String(setting_index-1));
    window.location.replace("/location.html");  // Or reload?
  }
}

function GoRight() {
  if (setting_index == setting_locations.length-1)
    window.location.replace("/settings.html");
  else {
    sessionStorage.setItem("location", String(setting_index+1));
    window.location.replace("/location.html");  // Or reload?
  }
}

function LoadDatabase() {
  setting_index = sessionStorage.getItem("location") || 0;
  setting_display_language = localStorage.getItem("display-lang");
  setting_forecast_language = localStorage.getItem("forecast-lang");
  setting_locations = JSON.parse(localStorage.getItem("locations"));
}

function LoadWeather() {
  if (setting_index >= setting_locations.length) {
    window.location.replace("/settings.html");
    return;
  }
  // TODO: cache the forecast
  // TODO: may want to preload the forecasts
  var request = new XMLHttpRequest({ mozSystem: true });
  var country = setting_locations[setting_index].country;
  var place = setting_locations[setting_index].place;
  var url = makeURL(setting_forecast_language, country, place);
  request.open('get', url, true);
  request.addEventListener('error', (ev) => showError(request.error || "URL request failed"));
  request.addEventListener('load', (ev) => WeatherLoaded(ev, loc));
  request.send();
}

function WeatherLoaded(ev, loc) {
  var request = ev.target;
  var response = request.responseXML;
  if(!response) {
    showError("Invalid reply from server");
    return;
  }
    
  try {
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
    "<div><b>" + loc + "</b></div>" +
    "<div><table style='width: 100%'><tbody>" +
    "<tr><td><b>Tid</b></td><td><b>C</b></td><td><b>Vaer</b></td><td><b>mm</b></td></tr>\n" + 
    forecast.map((x) => "<tr>" +
                 "<td>" + x.from[4] + "-" + x.to[4] + "</td> " +
                 "<td>" + x.temp + "</td> " +
                 "<td>" + x.summary + "</td> " +
                 "<td>" + (x.precip == 0 ? "----" : x.precip) +  "</td></tr>\n").join("") + 
    "</tbody></table></div>";
    
    results.innerHTML = computedResponse;
    results.hidden = false;
    settingsElt.hidden = false;
  }
  catch (e) {
    showError("Failed to parse response");
    return;
  }

  function get(e, ...ns) {
    for ( var i=0 ; i < ns.length ; i++ )
      e = e.getElementsByTagName(ns[i])[0];
    return e;
  }
}

function makeURL(forecast_language, country, place) {
  var c, d, x;
  switch (forecast_language) {
    default:
    case "Norsk":
      c = (country == "NO") ? "Norge" : no_countries[country];
      d = "varsel.xml";
      x = "sted";
      break;
    case "Nynorsk":
      c = (country == "NO") ? "Noreg" : no_countries[country];
      d = "varsel.xml";
      x = "stad";
      break;
    case "English":
      c = en_countries[country];
      d = "forecast.xml";
      x = "place";
      break;
  }
  return "http://www.yr.no/" + x + "/" + c + "/" + place + "/" + d;
}

function showError(text) {
  alert(text);
  results.hidden = true;
}
