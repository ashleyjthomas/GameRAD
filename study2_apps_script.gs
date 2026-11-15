/**
 * GameRAD Study 2 — data receiver.
 * Paste into Extensions > Apps Script of the Study 2 Google Sheet,
 * then Deploy > New deployment > Web app
 * (Execute as: Me / Who has access: Anyone), and copy the web-app URL
 * into SHEETS_URL in index_study2.html.
 */
const HEADERS = [
  'CHSResponseID','ParticipantID','ChildName','Age','Type','VignetteID',
  'VignetteModel','QuestionIndex','QuestionModel','ItemID','IsCongruent',
  'Answer','InitialYesNo','KeyedAnswer','Correct','ModelIDChosen',
  'ModelIDCorrect','ModelIDCertainty','Timestamp','ReceivedAt'
];

function doPost(e) {
  const lock = LockService.getScriptLock();
  lock.waitLock(20000);                       // avoid interleaved writes
  try {
    const sheet = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
    if (sheet.getLastRow() === 0) sheet.appendRow(HEADERS);
    const body = JSON.parse(e.postData.contents);
    const rows = body.rows || [];
    const now = new Date().toISOString();
    const out = rows.map(r =>
      HEADERS.slice(0, -1).map(h => r[h] !== undefined ? r[h] : '').concat([now]));
    if (out.length) {
      sheet.getRange(sheet.getLastRow() + 1, 1, out.length, HEADERS.length)
           .setValues(out);
    }
    return ContentService.createTextOutput(JSON.stringify({ok: true, n: out.length}))
                         .setMimeType(ContentService.MimeType.JSON);
  } finally {
    lock.releaseLock();
  }
}

function doGet() {   // quick sanity check: open the web-app URL in a browser
  return ContentService.createTextOutput('GameRAD Study 2 receiver is running.');
}
