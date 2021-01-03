const fs = require("fs");

const dirContent = fs.readdirSync(".");
const textFiles = dirContent.filter((filename) => filename.endsWith(".txt"));

for (const textFile of textFiles) {
  let markdownContents = "";
  const markdownFilename = textFile.replace(".txt", ".md");
  const fileContents = fs.readFileSync(`./${textFile}`, {
    encoding: "utf8",
    flag: "r",
  });
  const highlights = fileContents.split("\n\n");
  const bookName = highlights.shift();

  markdownContents += `type: #book\n`;
  markdownContents += `book: ${bookName}\n`;
  markdownContents += `\n`;

  for (const highlight of highlights) {
    const [quote, note] = highlight.trim().split("\n");
    const noteWithoutPrefix = note ? note.replace("Note: ", "").trim() : "";
    const trimmedQuote = quote ? quote.trim() : "";
    if (noteWithoutPrefix.toLowerCase().startsWith("chapter")) {
      const cleanNote = noteWithoutPrefix.replace(/[^0-9]/g, "");
      const chapterDepth = parseInt(cleanNote, 10) || 1;
      const markdownHeadings = "#".repeat(chapterDepth);
      markdownContents += `${markdownHeadings} ${trimmedQuote}\n\n`;
    } else {
      markdownContents += trimmedQuote ? `> ${trimmedQuote}\n\n` : "";
      markdownContents += noteWithoutPrefix ? `${noteWithoutPrefix}\n\n` : "";
    }
  }

  fs.writeFileSync(`./${markdownFilename}`, markdownContents);
}
