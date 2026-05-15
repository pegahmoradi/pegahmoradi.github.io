// ─────────────────────────────────────────────────────────────────────────────
// Qualitative Coder — Google Apps Script backend
//
// Setup:
//   1. Go to script.google.com → New project
//   2. Paste this entire file into the editor
//   3. Deploy → New deployment → Web app
//      Execute as: Me  |  Who has access: Anyone
//   4. Copy the Web App URL → paste into the site's ⚙ Settings panel
// ─────────────────────────────────────────────────────────────────────────────

var HEADERS = [
  'id', 'company_name', 'ticker', 'quarter',
  'section', 'speaker', 'role',
  'speech_act', 'codes', 'pegah_notes', 'timestamp'
];

function doPost(e) {
  try {
    var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
    var data  = JSON.parse(e.postData.contents);

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

    return ContentService
      .createTextOutput(JSON.stringify({ status: 'ok' }))
      .setMimeType(ContentService.MimeType.JSON);

  } catch (err) {
    return ContentService
      .createTextOutput(JSON.stringify({ status: 'error', message: err.message }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}
