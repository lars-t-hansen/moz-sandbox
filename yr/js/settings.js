window.addEventListener('DOMContentLoaded', InitSettings);

function InitSettings() {
  // Wrong.
  // If there are no locations in the list, stay here.
  // Otherwise go to the location we were at.
  document.getElementById('back_button').onclick = () => window.history.back();
}
