// ─────────────────────────────────────────────────────────────────────────────
// Qualitative Coder — Google Apps Script backend
//
// Setup:
//   1. Go to script.google.com → New project
//   2. Paste this entire file into the editor
//   3. Deploy → New deployment → Web app
//      Execute as: Me  |  Who has access: Anyone
//   4. URL is already hard-coded in index.html
//
// After any edits: Deploy → Manage deployments → Edit → New version → Deploy
// ─────────────────────────────────────────────────────────────────────────────

var SECRET  = '7968f8d10b24bb5c10c716733d19a62620e0adf305865865';
var SHEET_ID = '1ubsPUZ1i9Nah-X_-te125VYDvWZz5Q4g_f2eYIcYl1M';

var HEADERS = [
  'id', 'company_name', 'ticker', 'quarter',
  'section', 'speaker', 'role',
  'speech_act', 'codes', 'pegah_notes', 'timestamp'
];

// doGet — returns all saved codings so any device can sync on load
function doGet(e) {
  if (!e.parameter || e.parameter.secret !== SECRET) {
    return ContentService.createTextOutput(JSON.stringify({ status: 'unauthorized' }))
      .setMimeType(ContentService.MimeType.JSON);
  }
  try {
    var sheet  = SpreadsheetApp.openById(SHEET_ID).getActiveSheet();
    var values = sheet.getDataRange().getValues();

    if (values.length <= 1) {
      return ContentService.createTextOutput(JSON.stringify({ status: 'ok', codings: [], position: null }))
        .setMimeType(ContentService.MimeType.JSON);
    }

    var headers     = values[0];
    var idIdx       = headers.indexOf('id');
    var codesIdx    = headers.indexOf('codes');
    var notesIdx    = headers.indexOf('pegah_notes');

    var codings  = [];
    var position = null;

    for (var i = 1; i < values.length; i++) {
      var row = values[i];
      if (String(row[idIdx]) === '_position') {
        position = parseInt(row[codesIdx]) || 0;
        continue;
      }
      if (row[idIdx] === '' || row[idIdx] === null) continue;
      codings.push({
        id:          String(row[idIdx]),
        codes:       String(row[codesIdx] || '').split(',').map(function(t) { return t.trim(); }).filter(Boolean),
        pegah_notes: String(row[notesIdx] || ''),
      });
    }

    return ContentService.createTextOutput(JSON.stringify({ status: 'ok', codings: codings, position: position }))
      .setMimeType(ContentService.MimeType.JSON);

  } catch (err) {
    return ContentService.createTextOutput(JSON.stringify({ status: 'error', message: err.message }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}

// doPost — writes or updates a single coding row
function doPost(e) {
  try {
    var data = JSON.parse(e.postData.contents);
    if (data.secret !== SECRET) {
      return ContentService.createTextOutput(JSON.stringify({ status: 'unauthorized' }))
        .setMimeType(ContentService.MimeType.JSON);
    }

    var sheet = SpreadsheetApp.openById(SHEET_ID).getActiveSheet();

    // Write header row if the sheet is brand new
    if (sheet.getLastRow() === 0) {
      sheet.appendRow(HEADERS);
      SpreadsheetApp.flush();
    }

    var newRow = HEADERS.map(function(h) {
      return data[h] !== undefined ? data[h] : '';
    });

    // Check whether a row with this id already exists; if so, update it
    var values   = sheet.getDataRange().getValues();
    var idColIdx = HEADERS.indexOf('id');
    var foundRow = -1;

    for (var i = 1; i < values.length; i++) {
      if (String(values[i][idColIdx]) === String(data.id)) {
        foundRow = i + 1; // Sheets rows are 1-indexed
        break;
      }
    }

    if (foundRow > 0) {
      sheet.getRange(foundRow, 1, 1, newRow.length).setValues([newRow]);
    } else {
      sheet.appendRow(newRow);
    }

    return ContentService.createTextOutput(JSON.stringify({ status: 'ok' }))
      .setMimeType(ContentService.MimeType.JSON);

  } catch (err) {
    return ContentService.createTextOutput(JSON.stringify({ status: 'error', message: err.message }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}
