# dayone-convert
A JSON parser (and plain text file importer) for Day One.

## Usage

The Day One importer expects a ZIP file and takes the journal name from it.

If you have a directory of text files whose modification dates reflect the dates
you want them to have in your journal (such as an export from nvALT), run the
following commands replacing "~/directory/" with that directory path.

```sh
stack exec dayone ~/directory/ > journal.json
zip -r "Journal Title.zip" journal.json
```
