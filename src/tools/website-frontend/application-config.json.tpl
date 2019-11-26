{
    "backend": {
        "root": "http://localhost:8080/"
    },
    "website": {
        "url": "http://localhost:9000/",
        "content_root": "website/",
        "target_file_dir_links": ["README.md", "README", "readme.md", "readme"],
        "slogan": {
            "rotation_interval": 2500
        },
        "defaults": {
            "entry": {
                "form": {
                    "organization": "none",
                    "scope": "v1"
                }
            }
        },
        "formats": {
            "preferred": ["line", "ni", "xmltool"]
        }
    },
    "github": {
        "website": {
            "root": "https://github.com/",
            "paths": {
                "issues": "ElektraInitiative/libelektra/issues/new",
                "doc_root": "ElektraInitiative/libelektra/tree/master/",
                "img_root": "ElektraInitiative/libelektra/raw/master/"
            }
        }
    },
    "translations": {
        "enabled": ["en"],
        "mappings": {
            "en_US": "en",
            "en_UK": "en"
        },
        "default": "en"
    },
    "logger": {
        "enabled": true
    },
    "jwt": {
        "validity": 30
    }
}
