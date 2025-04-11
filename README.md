# Build Instructions

This project depends on the following packages published to github.com/harrisaoz:
- FsConfigLoader
- FsSimpleFileIO

In order to retrieve these packages, read access to the packages must be obtained or
the projects can be built and packaged locally.

# Usage

./SaveAttachments/publish/SaveAttachments.exe [json-config-file]

The configuration file defaults to ProcessAttachments.json.

See TypedConfiguration/TypedConfiguration.fs for the supported configuration
parameters.

## Important Note

The algorithm used to name each exported attachment depends implicitly on how many source folders
are listed in the configuration file under ```Mailbox/SourceFolders```. If only one folder is
specified, then the naming follows the pattern ```<sender>_<timestamp>__<filename>```
(TimesheetNaming). Otherwise, the name is constructed according to the pattern
```<source-folder>_<timestamp>_<filename>``` (InvoiceNaming), where ```<source-folder>``` is the
full name of the folder, with slashes replaced by double underscores (```__```). See
ImapKit/AttachmentNaming.fs:{TimesheetNaming|InvoiceNaming} for the implementation of each.

## Example JSON Configuration File

```json
{
  "Logging": {
    "LogDir": "D:\\logs\\attachment-export",
    "InfoFilename": "example-info.log",
    "ErrorFilename": "example-error.log",
    "TraceFilename": "example-trace.log",
    "ReportFilename": "example-report.log"
  },
  "ImapService": {
    "Provider": "gmail",
    "Endpoint": {
      "Hostname": "some-imap-server.example.com",
      "Port": "993"
    },
    "Credentials": {
      "Username": "some-user@not-a-real-domain.example.com",
      "Password": "some-long-and-complex-password"
    }
  },
  "Mailbox": {
    "SourceFolders": [
      "A-Top-Level-Folder",
      "Other-Top-Level-Folder/A-Subfolder"
    ],
    "OkSubfolder": "Processed",
    "ErrorSubfolder": "Attention"
  },
  "Export": {
    "DestinationFolder": "\\\\some-fileserver\\a-fileshare\\processed-attachments"
  },
  "Categorisation": {
    "AcceptedMimeTypes": [
      "application/pdf",
      "application/octet-stream",
      "image/png"
    ],
    "IgnoredMimeTypes": [
      "text/html",
      "text/plain",
      "text/comma-separated-values",
      "application/pkcs7-signature",
      "text/csv"
    ],
    "IgnoreFilename": {
      "Contains": [
        "some-filename-pattern-that-we-want-to-exclude",
        "another-attachment-filename-pattern-to-ignore"
      ],
      "EndsWith": [
        ".zip",
        ".exe"
      ]
    }
  }
}
```
