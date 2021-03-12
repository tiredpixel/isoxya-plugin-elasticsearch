# Isoxya plugin: Elasticsearch

[Isoxya plugin: Elasticsearch](https://github.com/isoxya/isoxya-plugin-elasticsearch) is an open-source (BSD 3-Clause) streamer plugin for [Isoxya](https://www.isoxya.com/) web crawler. This plugin uses Isoxya 2 JSON interfaces to stream data into an [Elasticsearch](https://www.elastic.co/elasticsearch/) cluster, making it possible to query using all the normal features provided by Elasticsearch and [Kibana](https://www.elastic.co/kibana).

Since Isoxya supports both processor and streamer plugins using the Isoxya interfaces, it's not actually necessary to use this plugin at all, opening up the possibility of streaming to different datastores such as [PostgreSQL](https://www.postgresql.org/), [Apache Hadoop](https://hadoop.apache.org/), or [AWS Redshift](https://aws.amazon.com/redshift/) instead—or even not persisting the data at all, and connecting it to a web app using WebSockets, or alternatively to some alerting system.


## Features

- index auto-creation using organisation and date
  `isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01`

- insert using [Elasticsearch Bulk API](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html)
  `Content-Type: application/x-ndjson`

- deterministic auto-generated document ids
  `9c8100c7642a06acc892c9696e55789ec0dd67ad0dee06a5c378343b5e47a969.1`

- one-to-many support for crawled pages which result in multiple documents, based on plugin tag
  `plug_proc.tag: spellchecker`

- document metadata for position within sequence
  `data_i`, `data_n`


## Setup (Elastic Stack)

### Auth

- create role `isx_plug_elasticsearch`
  - Index privileges
    - Indices
      - `isoxya.*`
    - Privileges
      - `index`
      - `create_index`

- create user `isx_dev_plug_elasticsearch`
  - Roles
    - `isx_plug_elasticsearch`

- set in `ELASTICSEARCH_HOST` using HTTP Basic Auth (`eg_user:PASSWORD@`)

### Kibana

- Management
  - Kibana
    - Index Patterns
      - Create Index Pattern
        - Index Pattern: `isoxya.*`
        - Time Filter Field: `t_retrieval`


## Contact

[tiredpixel.com](https://www.tiredpixel.com/) · [tp@tiredpixel.com](mailto:tp@tiredpixel.com)

LinkedIn: [in/nic-williams](https://www.linkedin.com/in/nic-williams/) · GitHub: [tiredpixel](https://github.com/tiredpixel)


## Licence

Copyright © 2019-2021 [Nic Williams](https://www.tiredpixel.com/). It is free software, released under the BSD 3-Clause licence, and may be redistributed under the terms specified in `LICENSE`.
