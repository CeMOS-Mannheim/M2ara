// M2ara UI behaviour: feature selection, keyboard shortcuts, share links.
//
// The state of the feature comparison lives here (keyed by mzIdx), so ticking a
// checkbox never re-renders the table and the selection survives paging,
// filtering and sorting.
//
// The script is loaded in the page head, i.e. before Shiny and jQuery are
// guaranteed to exist, so everything that needs them is registered in
// m2araInit().

window.m2araSelected = window.m2araSelected || [];
window.m2araColours = window.m2araColours || ['#D55E00', '#009E73', '#CC79A7', '#0072B2'];

// restore checkboxes, disabled state and row colours from the state
window.m2araUpdateCheckboxes = function () {
  var sel = window.m2araSelected || [];
  var full = sel.length >= 4;

  jQuery('#mzTable input.m2ara-check').each(function () {
    var idx = parseInt(this.value, 10);
    var checked = sel.indexOf(idx) >= 0;
    this.checked = checked;
    this.disabled = (!checked && full);
  });

  jQuery('#mzTable tbody tr').each(function () {
    var boxes = jQuery(this).find('input.m2ara-check');
    if (boxes.length === 0) { return; }
    var pos = sel.indexOf(parseInt(boxes.val(), 10));
    if (pos >= 0 && sel.length === 1) {
      this.style.backgroundColor = 'rgba(0, 0, 0, 0.06)';
      this.style.borderLeft = '4px solid #999999';
    } else if (pos >= 0) {
      var hex = window.m2araColours[pos];
      var r = parseInt(hex.substr(1, 2), 16);
      var g = parseInt(hex.substr(3, 2), 16);
      var b = parseInt(hex.substr(5, 2), 16);
      this.style.backgroundColor = 'rgba(' + r + ',' + g + ',' + b + ',0.18)';
      this.style.borderLeft = '4px solid ' + hex;
    } else {
      this.style.backgroundColor = '';
      this.style.borderLeft = '';
    }
  });
};

function m2araSendSelection() {
  Shiny.setInputValue('mzTable_selected', (window.m2araSelected || []).slice(),
                      {priority: 'event'});
}

function m2araRedrawTable() {
  var table = jQuery('#mzTable table').DataTable();
  if (table && table.rows) { table.rows().invalidate('data').draw(false); }
}

function m2araClick(id) {
  var el = document.getElementById(id);
  if (el && !el.disabled) { el.click(); }
}

function m2araFocusSearch() {
  var box = jQuery('#mzTable .dataTables_filter input').first();
  if (box.length === 0) { box = jQuery('#mzTable input[type=search]').first(); }
  box.focus();
  if (box.select) { box.select(); }
}

// keyboard shortcuts ---------------------------------------------------------
function m2araKeydown(e) {
  var key = (e.key || '').toLowerCase();
  var ctrl = e.ctrlKey || e.metaKey;
  var target = e.target || {};
  var typing = /^(input|textarea|select)$/i.test(target.tagName) || target.isContentEditable;

  if (ctrl && e.key === 'Enter') {
    e.preventDefault();
    m2araClick('process');
    return;
  }
  if (ctrl && !e.shiftKey && key === 'f') {
    e.preventDefault();
    m2araFocusSearch();
    return;
  }
  if (ctrl && !e.shiftKey && key === 's') {
    e.preventDefault();
    m2araClick('downloadPlot');
    return;
  }
  if (ctrl && !e.shiftKey && key === 'e') {
    e.preventDefault();
    m2araClick('exportRun');
    return;
  }
  if (ctrl && !e.shiftKey && key === 'd') {
    e.preventDefault();
    m2araClick('clearSelection');
    return;
  }
  if (ctrl && e.shiftKey && /^[1-4]$/.test(key)) {
    e.preventDefault();
    var row = jQuery('#mzTable tbody tr:eq(' + (parseInt(key, 10) - 1) + ')');
    var box = row.find('input.m2ara-check');
    if (box.length > 0 && !box.prop('disabled')) { box.trigger('click'); }
    return;
  }
  if (ctrl && key === '/') {
    e.preventDefault();
    jQuery('#m2araShortcuts').toggle();
    return;
  }
  if (key === 'escape' && !typing) {
    m2araClick('clearSelection');
  }
}

// registration ---------------------------------------------------------------
function m2araInit() {
  if (!window.jQuery || !window.Shiny) {
    window.setTimeout(m2araInit, 50);
    return;
  }

  jQuery(document).on('change', '#mzTable input.m2ara-check', function () {
    var idx = parseInt(this.value, 10);
    var sel = window.m2araSelected || [];
    if (this.checked) {
      if (sel.length >= 4) { this.checked = false; return; }
      if (sel.indexOf(idx) < 0) { sel.push(idx); }
    } else {
      sel = sel.filter(function (v) { return v !== idx; });
    }
    window.m2araSelected = sel;
    window.m2araUpdateCheckboxes();
    m2araSendSelection();
    m2araRedrawTable();
  });

  // server side control of the selection
  Shiny.addCustomMessageHandler('m2ara_reset_selection', function (msg) {
    window.m2araSelected = [];
    window.m2araUpdateCheckboxes();
    m2araSendSelection();
    m2araRedrawTable();
  });

  Shiny.addCustomMessageHandler('m2ara_set_selection', function (msg) {
    window.m2araSelected = (msg || []).map(function (v) { return parseInt(v, 10); });
    window.m2araUpdateCheckboxes();
    m2araSendSelection();
    m2araRedrawTable();
  });

  // show the share link; copying happens in the click handler because the
  // clipboard API refuses writes that are not triggered by a user gesture
  Shiny.addCustomMessageHandler('m2ara_show_link', function (msg) {
    var box = document.getElementById('m2araLinkBox');
    var input = document.getElementById('m2araLinkInput');
    if (!box || !input) { return; }
    input.value = msg.url;
    box.style.display = 'block';
    input.focus();
    input.select();
  });

  jQuery(document).on('click', '#m2araLinkCopy', function () {
    var input = document.getElementById('m2araLinkInput');
    if (!input) { return; }
    var url = input.value;
    var done = function () {
      jQuery('#m2araLinkHint').text('Link copied to the clipboard.');
    };
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(url).then(done, function () {
        input.focus(); input.select(); document.execCommand('copy'); done();
      });
    } else {
      input.focus(); input.select(); document.execCommand('copy'); done();
    }
  });

  jQuery(document).on('click', '#m2araLinkClose', function () {
    jQuery('#m2araLinkBox').hide();
  });

  jQuery(document).on('keydown', m2araKeydown);
}

m2araInit();
