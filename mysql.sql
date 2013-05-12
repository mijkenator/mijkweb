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
    created     timestamp not null default current_timestamp,
    active      int not null default 1,
    primary key (id),
    unique key (org_id),
    unique key (login),

key (login, password)
) Engine=InnoDB default charset=utf8 comment='for sytem users';

DROP TABLE if exists system_user_profile;
CREATE TABLE system_user_profile
(
    sysaccid    int not null,
    app_user_count  int not null default 0,
    storage_keys_count  int not null default 0,
    max_app_users   int not null default 1000,
    max_keys_inst   int not null default 1000,
    key(sysaccid)
) Engine=InnoDB default charset=utf8 comment='profile table for system users';
--
-- system_user after insert trigger -> to create empty brand new profile  
--
DROP TRIGGER IF EXISTS system_user_ai;
DROP TRIGGER IF EXISTS system_user_ad;
DELIMITER ;;
create trigger system_user_ai after insert on system_user for each row
begin
   insert into system_user_profile (sysaccid) values (new.id);
end;;
create trigger system_user_ad after delete on system_user for each row
begin
   delete from system_user_profile where sysaccid = old.id;
end;;
delimiter ;

DROP TABLE if exists organization;
CREATE TABLE organization
(
    id          int not null auto_increment,
    guid        varchar(50) not null,
    properties  varchar(20000),
    primary key (id),
    unique key (guid)
) Engine=InnoDB default charset=utf8 comment='organization table';

DROP TABLE if exists app_user_instance;
DROP TABLE if exists app_user;
CREATE TABLE app_user
(
    id          int not null auto_increment,
    login       varchar(50) not null,
    password    varchar(50) not null,
    sysaccid    int not null,
    remote_ip   varchar(50),
    register_time       timestamp not null default current_timestamp,
    last_login_time     timestamp,
    active      int not null default 1,
    primary key (id),
    key (sysaccid),
    unique key (login, sysaccid),
    key (login, password)
) Engine=InnoDB default charset=utf8 comment='for app users';

drop trigger if exists app_user_ai;
drop trigger if exists app_user_ad;
delimiter ;;
create trigger app_user_ai after insert on app_user for each row
begin
    update system_user_profile
        set app_user_count = app_user_count + 1 
        where sysaccid = new.sysaccid;
end;;
create trigger app_user_ad after delete on app_user for each row
begin
    update system_user_profile
        set app_user_count = app_user_count - 1 
        where sysaccid = old.sysaccid;
end;;
delimiter ;

DROP TABLE if exists app_user_instance;
CREATE TABLE app_user_instance
(
    id           int not null auto_increment,
    appuserid    int not null,
    apptype      enum('IPHONE','ANDROID','WEBAPP') default 'WEBAPP',
    deviceid     varchar(255) default null,
    primary key (id),
    key `appuserid` (appuserid),
    key (apptype),
    constraint `app_u_i_fk_uid` foreign key (`appuserid`)  references `app_user` (`id`) 
        on delete cascade 
        on update cascade
) Engine=InnoDB default charset=utf8 comment='for app users instances';

DROP TABLE if exists user_key_storage;
CREATE TABLE user_key_storage
(
    sysaccid    int not null,
    `key`       varchar(250),
    `value`     varchar(65000),
    unique key (sysaccid, `key`)
) Engine=InnoDB default charset=utf8 comment='key storage';

-- store procedure for update app user info
DROP PROCEDURE IF EXISTS updateappuser;
DROP PROCEDURE IF EXISTS deleteappuser;
delimiter ;;
CREATE PROCEDURE updateappuser (
    var_login varchar(255),
    var_password varchar(255),
    var_active int,
    var_sysaccount_id int,
    var_appuser_id int)
BEGIN
    update app_user 
        set
            login = var_login,
            password = var_password,
            active = var_active
        where 
            sysaccid = var_sysaccount_id and
            id = var_appuser_id;
    SELECT 1 as 'ret';
END;;
CREATE PROCEDURE deleteappuser (
    var_sysaccount_id int,
    var_appuser_id int)
BEGIN
    delete from app_user 
        where 
            sysaccid = var_sysaccount_id and
            id = var_appuser_id;
    select 1 as 'ret';
END;;
delimiter ;


DROP TABLE if exists sysacc_stat;
CREATE TABLE sysacc_stat
(
    sysaccid    int not null,
    raw_stat    varchar(65000),
    primary key (sysaccid)
) Engine=InnoDB default charset=utf8 comment='sys accs raw stats';
DROP TABLE if exists sysacc_stat;
CREATE TABLE sysacc_stat_history
(
    sysaccid    int not null,
    raw_stat    varchar(65000),
    dtime       timestamp DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    primary key (sysaccid)
) Engine=InnoDB default charset=utf8 comment='sys accs raw stats history table';
DROP PROCEDURE IF EXISTS sync_sysacc_stat;
DROP PROCEDURE IF EXISTS sync_sysacc_stat;
delimiter ;;
CREATE PROCEDURE sync_sysacc_stats (
    var_account_id int,
    var_stat_blob varchar(65000))
BEGIN
    replace into sysacc_stat (sysaccid, raw_stat) values (var_account_id, var_stat_blob);
    SELECT 1 as 'ret';
END;;

delimiter ;
DROP PROCEDURE IF EXISTS flush_reset;
delimiter ;;
CREATE PROCEDURE flush_reset (
BEGIN
    lock tables sysacc_stat write;
    insert into sysacc_stat_history th (th.sysaccid, th.raw_stat) select tr.sysaccid, tr.raw_stat from sysacc_stat; 
    delete from sysacc_stat;
    unlock tables;
    SELECT 1 as 'ret';
END;;
delimiter ;
