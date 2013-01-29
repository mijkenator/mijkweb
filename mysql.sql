USE mijkweb;
DROP TABLE if exists system_user;
CREATE TABLE system_user
(
    id          int not null auto_increment,
    login       varchar(50) not null,
    password    varchar(50) not null,
    email       varchar(50) not null,
    org_id      int not null,
    remote_ip   varchar(50),
    is_verified int,
    primary key (id),
    unique key (org_id),
    unique key (login),
    key (login, password)
) Engine=InnoDB default charset=utf8 comment='for sytem users';

DROP TABLE if exists organization;
CREATE TABLE organization
(
    id          int not null auto_increment,
    guid        varchar(50) not null,
    properties  varchar(20000),
    primary key (id),
    unique key (guid)
) Engine=InnoDB default charset=utf8 comment='organization table';

DROP TABLE if exists app_user;
CREATE TABLE app_user
(
    id          int not null auto_increment,
    login       varchar(50) not null,
    password    varchar(50) not null,
    sysaccid    int not null,
    remote_ip   varchar(50),
    primary key (id),
    key (sysaccid),
    unique key (login, sysaccid),
    key (login, password)
) Engine=InnoDB default charset=utf8 comment='for app users';
