window.addEventListener('DOMContentLoaded', SearchLogic);

function SearchLogic() {
  document.getElementById('search_button').onclick = DoSearch;
  document.getElementById('done_button').onclick = DoDone;
}

function DoDone() {
  window.location.replace("/locations.html");
}

// When a single letter is searched for we look only in the single-letter file, if it exists,
// we do not consider the letter as a prefix of something longer.  This is a special case, but
// reasonable.

function DoSearch() {
  var term = document.getElementById('search_term').text; // ???
  // Launder the term: remove leading spaces and anything after the first string of
  // letters.
  // TODO: canonical case, of course, for both term and filename
  var filename = term.substring(0,Math.min(2,term.length));
  if (filename == "")
    return;
  var request = new XMLHttpRequest();
  // anything to ask for the format?  What we get back is not XML, but a string
  request.open('get', "/data/loc-" + filename + ".data", true); // what is true?
  request.addEventListener('error', IndexFileMissing);
  request.addEventListener('load', (ev) => IndexFileLoaded(ev, term));
  request.open('get', url, true);
}

function IndexFileMissing() {
  displayResults([], false);
}

function IndexFileLoaded(ev, term) {
  var text = FIXME;
  var results = [];
  var idx = 0;
  term = "*" + term;
  // TODO: a cutoff?
  while ((k = text.indexOf(term, idx)) != -1) {
    results.push(FIXME);
  }
  displayResults(results, false);
}

function DisplayResults(results, toomany) {
  
}
