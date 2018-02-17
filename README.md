# dayone-convert
A JSON parser (and plain text file importer) for Day One.

## Usage

First, build the executable with:

```
stack build
``` 

### Importing into Day One

The Day One importer expects a ZIP file and takes the journal name from it.

If you have a directory of text files whose modification dates reflect the dates
you want them to have in your journal (such as an export from nvALT), run the
following commands replacing "~/directory/" with that directory path.

```sh
stack exec dayone -- from-nvalt ~/directory/ journal.json
zip -r "Journal Title.zip" journal.json
```

### Exporting from Day One JSON

If you have a Day One JSON file, you can convert it into flat files of the form
"2018/02/17.txt" (multiple entries on the same day are concatenated with
newlines separating them).

```sh
unzip "2018-02-17-Journal Title.zip"  # will have "Journal.json" in output
stack exec dayone -- to-files Journal.json ~/directory/  # output entries to ~/directory
```
