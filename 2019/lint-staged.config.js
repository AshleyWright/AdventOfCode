module.exports = {
  "src/**/*.purs": filenames =>
    filenames.flatMap(filename => [
      `purty --write "${filename}"`,
      `git add "${filename}"`
    ]),
  "test/**/*.purs": filenames =>
    filenames.flatMap(filename => [
      `purty --write "${filename}"`,
      `git add "${filename}"`
    ])
};
