# Isoxya plugin Elasticsearch

Isoxya plugin Elasticsearch streams data into an [Elasticsearch](https://www.elastic.co/elasticsearch/) cluster, making it possible to query data using the advanced reporting features of Elasticsearch and [Kibana](https://www.elastic.co/kibana). It is a plugin for [Isoxya](https://www.isoxya.com/) web crawler.

https://hub.docker.com/r/isoxya/isoxya-plugin-elasticsearch  
https://github.com/tiredpixel/isoxya-plugin-elasticsearch  


## Features

- index auto-creation using date
  `isoxya.2021-12-02`

- insert using [Elasticsearch Bulk API](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html)
  `Content-Type: application/x-ndjson`

- deterministic auto-generated document ids
  `9c8100c7642a06acc892c9696e55789ec0dd67ad0dee06a5c378343b5e47a969.1`

- one-to-many support for crawled pages which result in multiple documents, based on plugin tag
  `processor.tag: spellchecker`

- document metadata for position within sequence
  `data_i`, `data_n`


## Installation

Compile and boot locally:

```sh
docker compose up
```

Images are also published using the `latest` tag (for development), and version-specific tags (for production). Do *not* use a `latest` tag in production!


## Setup (Elastic Stack)

### Auth

- create role `isoxya_plugin_elasticsearch`
  - Index privileges
    - Indices
      - `isoxya.*`
    - Privileges
      - `index`
      - `create_index`

- create user `isoxya_plugin_elasticsearch_dev`
  - Roles
    - `isoxya_plugin_elasticsearch`

- set in `ELASTICSEARCH_HOST` using HTTP Basic Auth (`eg_user:PASSWORD@`)

### Kibana

- Management
  - Kibana
    - Index Patterns
      - Create Index Pattern
        - Index Pattern: `isoxya.*`
        - Time Filter Field: `retrieved`


## Licence

Copyright © [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
