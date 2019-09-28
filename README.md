# Isoxya Pipeline Elasticsearch (Haskell)

[Isoxya Pipeline Elasticsearch](https://github.com/pavouk-0/isoxya-pipeline-elasticsearch-hs) is an Isoxya Pipeline streaming data to an Elasticsearch cluster. Using this in combination with the proprietary Isoxya engine, it's possible to crawl entire websites efficiently, even if they have millions of pages, and process them in myriad ways, depending on which plugins it's combined with, querying them using all the normal features provided by Elasticsearch, and visually using Kibana. [Docker images](https://hub.docker.com/r/pavouk0/isoxya-pipeline-elasticsearch) are available.

[Isoxya](https://www.pavouk.tech/category/isoxya/) is a High-Performance Internet Data Processor and Web Crawler. It is designed as a next-generation web crawler, scalable for large sites (millions of pages), cost-effective for tiny sites (1+ pages), offering flexible data processing using multi-industry plugins, delivering results via data streaming to multiple storage backends. It is magicked via a REST API using JSON, and is available now for private preview.


## Setup (Elastic Stack)

### Auth

- create role `isx_pipe`
  - Index privileges
    - Indices
      - `isoxya-*`
    - Privileges
      - `index`
      - `create_index`

- create user `eg-user`
  - Roles
    - `isx_pipe`

- set in `ELASTICSEARCH_HOSTS` using HTTP Basic Auth (`eg-user:PASSWORD@`)

### Kibana

- Management
  - Kibana
    - Index Patterns
      - Create Index Pattern
        - Index Pattern: `isoxya-*`
        - Time Filter Field: `t_retrieval`


## Blessing

May you find peace, and help others to do likewise.


## Contact

We've tried to make this document clear and accessible. If you have any feedback about how we could improve it, or if there's any part of it you'd like to discuss or clarify, we'd love to hear from you. Our contact details are:

Pavouk OÜ | [https://www.pavouk.tech/](https://www.pavouk.tech/) | [en@pavouk.tech](mailto:en@pavouk.tech)


## Licence

Copyright © 2019 [Pavouk OÜ](https://www.pavouk.tech/). It is free software, released under the BSD3 licence, and may be redistributed under the terms specified in `LICENSE`.
