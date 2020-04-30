# Isoxya plugin: Elasticsearch

[Isoxya plugin: Elasticsearch](https://github.com/pavouk-0/isoxya-plugin-elasticsearch) is a plugin for [Isoxya](https://www.pavouk.tech/isoxya/)—Web Crawler & Data Processing System. Using this in combination with the proprietary crawling engine by [Pavouk](https://www.pavouk.tech/), it's possible to stream data to an [Elasticsearch](https://www.elastic.co/elasticsearch/) cluster, querying it using all the normal features provided by Elasticsearch and [Kibana](https://www.elastic.co/kibana).


## Setup (Elastic Stack)

### Auth

- create role `isx_plugin_elasticsearch`
  - Index privileges
    - Indices
      - `isoxya.*`
    - Privileges
      - `index`
      - `create_index`

- create user `eg_user`
  - Roles
    - `isx_plugin_elasticsearch`

- set in `ELASTICSEARCH_HOSTS` using HTTP Basic Auth (`eg_user:PASSWORD@`)

### Kibana

- Management
  - Kibana
    - Index Patterns
      - Create Index Pattern
        - Index Pattern: `isoxya.*`
        - Time Filter Field: `t_retrieval`


## Blessing

May you find peace, and help others to do likewise.


## Contact

Pavouk OÜ | [https://www.pavouk.tech/](https://www.pavouk.tech/) | [en@pavouk.tech](mailto:en@pavouk.tech)


## Licence

Copyright © 2019-2020 [Pavouk OÜ](https://www.pavouk.tech/). It is free software, released under the BSD3 licence, and may be redistributed under the terms specified in `LICENSE`.
