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

// doGet — write mode (action=write) OR read mode (default)
function doGet(e) {
  if (!e.parameter || e.parameter.secret !== SECRET) {
    return ContentService.createTextOutput(JSON.stringify({ status: 'unauthorized' }))
      .setMimeType(ContentService.MimeType.JSON);
  }

  // ── Write mode: ?secret=...&action=write&data=[...rows] ──────────────────
  if (e.parameter.action === 'write') {
    try {
      var rows  = JSON.parse(e.parameter.data);
      var sheet = SpreadsheetApp.openById(SHEET_ID).getActiveSheet();

      if (sheet.getLastRow() === 0) {
        sheet.appendRow(HEADERS);
        SpreadsheetApp.flush();
      }

      var values       = sheet.getDataRange().getValues();
      var idColIdx     = HEADERS.indexOf('id');
      var tickerColIdx = HEADERS.indexOf('ticker');
      var codesColIdx  = HEADERS.indexOf('codes');
      var notesColIdx  = HEADERS.indexOf('pegah_notes');
      var tsColIdx     = HEADERS.indexOf('timestamp');

      var idToRowNum = {};
      for (var i = 1; i < values.length; i++) {
        var rowId = String(values[i][idColIdx]);
        var key   = rowId === '_position'
          ? '_position'
          : String(values[i][tickerColIdx] || '') + '_' + rowId;
        idToRowNum[key] = i + 1;
      }

      for (var r = 0; r < rows.length; r++) {
        var item    = rows[r];
        var itemKey = String(item.id) === '_position'
          ? '_position'
          : String(item.ticker || '') + '_' + String(item.id);
        var existing = idToRowNum[itemKey];
        if (existing) {
          // Update only coding columns so speech_act etc. are preserved
          sheet.getRange(existing, codesColIdx + 1).setValue(item.codes      !== undefined ? item.codes      : '');
          sheet.getRange(existing, notesColIdx + 1).setValue(item.pegah_notes !== undefined ? item.pegah_notes : '');
          sheet.getRange(existing, tsColIdx    + 1).setValue(item.timestamp   !== undefined ? item.timestamp   : '');
        } else {
          var newRow = HEADERS.map(function(h) { return item[h] !== undefined ? item[h] : ''; });
          sheet.appendRow(newRow);
          idToRowNum[itemKey] = sheet.getLastRow();
        }
      }

      return ContentService.createTextOutput(JSON.stringify({ status: 'ok' }))
        .setMimeType(ContentService.MimeType.JSON);
    } catch(err) {
      return ContentService.createTextOutput(JSON.stringify({ status: 'error', message: err.message }))
        .setMimeType(ContentService.MimeType.JSON);
    }
  }

  // ── Read mode (default) ──────────────────────────────────────────────────
  try {
    var sheet  = SpreadsheetApp.openById(SHEET_ID).getActiveSheet();
    var values = sheet.getDataRange().getValues();

    if (values.length <= 1) {
      return ContentService.createTextOutput(JSON.stringify({ status: 'ok', codings: [], position: null }))
        .setMimeType(ContentService.MimeType.JSON);
    }

    var headers     = values[0];
    var idIdx       = headers.indexOf('id');
    var tickerIdx   = headers.indexOf('ticker');
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
        ticker:      String(row[tickerIdx] || ''),
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

// doPost — writes or updates a batch of rows in one request
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

    // Support both a batch array (rows) and a single row (legacy)
    var rows = data.rows ? data.rows : [data];

    // Read all existing ids once for efficient upserts.
    // Key = ticker_id (compound) to avoid collisions across companies that share a numeric id.
    var values      = sheet.getDataRange().getValues();
    var idColIdx    = HEADERS.indexOf('id');
    var tickerColIdx = HEADERS.indexOf('ticker');
    var idToRowNum  = {};
    for (var i = 1; i < values.length; i++) {
      var rowId = String(values[i][idColIdx]);
      var key   = rowId === '_position'
        ? '_position'
        : String(values[i][tickerColIdx] || '') + '_' + rowId;
      idToRowNum[key] = i + 1; // 1-indexed
    }

    for (var r = 0; r < rows.length; r++) {
      var item   = rows[r];
      var newRow = HEADERS.map(function(h) {
        return item[h] !== undefined ? item[h] : '';
      });
      var itemKey  = String(item.id) === '_position'
        ? '_position'
        : String(item.ticker || '') + '_' + String(item.id);
      var existing = idToRowNum[itemKey];
      if (existing) {
        sheet.getRange(existing, 1, 1, newRow.length).setValues([newRow]);
      } else {
        sheet.appendRow(newRow);
        idToRowNum[itemKey] = sheet.getLastRow();
      }
    }

    return ContentService.createTextOutput(JSON.stringify({ status: 'ok' }))
      .setMimeType(ContentService.MimeType.JSON);

  } catch (err) {
    return ContentService.createTextOutput(JSON.stringify({ status: 'error', message: err.message }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}
