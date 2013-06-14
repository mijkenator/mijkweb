var PRCounter = 1;
var SysAccountId = 0;
var ListeningChannels = [];
var UserChannels = [];

$(document).ready(function() {
    $.ajax({
        type:   'POST',
        url:    '/_mijkweb/auth/check/session',
        data:   "request=" + JSON.stringify({ type: 'check_session' }), 
        success:   function(data){
            var obj = JSON.parse(data);
            if(!obj || !obj.login_type || obj.login_type!=1){
                window.location='/login'
            }else{
                SysAccountId = obj.sysaccid;
                ListeningChannels.push(['channel_'+SysAccountId, 'Common']);

                load_content();
            }
        }, 
        error:     function(data){window.location='/login'}
    });
    $("#menu").menu();
    $("#event_log").dialog({
        collapseEnabled: true,
        width: 550,
        height: 300,
        position: [806, 275],
        autoOpen: false,
        open: function(event, ui){ $("#event_log_tb").remove()},
        beforeCollapse: function(event, ui){ return move_to_toobox('#event_log', 'Event logs'); }
    });
    $("#message_box").dialog({ 
        collapseEnabled: true,
        autoOpen: false,
        width:'auto',
        open: function(event, ui){ $("#message_box_tb").remove()},
        beforeCollapse: function(event, ui){ return move_to_toobox('#message_box', 'Messages'); }
    });
    $("#loginb").click( form_submit );
    $("#to_f").autocomplete({
        source: channel_autocomplete,
        delay: 500,
        select: function(event, ui){
                $("#to_f").val( ui.item.label );
                $("#to_f_h").val( ui.item.value );
                return false;
            },
        focus: function(event, ui){
                $("#to_f").val( ui.item.label );
                $("#to_f_h").val( ui.item.value );
                return false;
            }
    });
    settings_dialog();
    get_stat_mkh();
});

function load_content(){
    polling_process(0);
    init_grid();    
}

function polling_process(seqN){
    $.ajax({
        type:   'POST',
        url:    '/_mijkweb/auth/poll',
        data: "request=" + JSON.stringify({type:'poll',seq: seqN, channels: 
            $.map(ListeningChannels, function(val, i){ return val[0]})}),
        success:   function(data){
            PRCounter++;
            var obj = JSON.parse(data);
            if(obj && (obj.seq || obj.seq == 0)){
                console.log("Poll OK:", data);
                add_event_log(data);
                update_top_stats(obj);
                polling_process(obj.seq)
            }else{
                console.log('Unexpect poll result', data)
            }
        }, 
        error:     function(data){ console.log("Error polling", data) }
    });
}

function init_grid(){
    jQuery("#list2").jqGrid({
        url:'/_mijkweb/auth/admin/grid',
        height: 470,
        mtype: 'POST',
        datatype: "json",
        postData: {type:'select'}, // setGridParam to change dynamically
        jsonReader: {
            root: 'rows',
            id: 'id',
            cell: 'cell'
        },
        colNames:['Id','Active', 'Login', 'Password', 'IP Address', 'Created time', 'Last Login Time'],
        colModel:[
            {name:'id',index:'id', width:55, align:'center', editable:false, editoptions:{readonly:true,size:10}},
            {name:'active',index:'active', width:55, align:'center', 
                editable:true, edittype:"select", editoptions:{value:"1:Yes;0:No"}, formatter:active_cell_formatter},
            {name:'login',index:'login', width:100, align:'center', editable:true, 
                    editrules:{required:true,custom:true,custom_func: check_field}},
            {name:'password', sortable:false, width:100, align:'center', editable:true, 
                    editrules:{required:true,custom:true,custom_func: check_field}},
            {name:'ip_address',index:'remote_ip', width:100, align:"center", editable:false},
            {name:'register_time', index:'register_time',  width:150, align:'center', editable:false},
            {name:'last_login_time',index:'last_login_time', width:150, align:'center', editable:false}
        ],
        rowNum:20,
        rowList:[20,30,50,100],
        pager: '#pager2',
        sortname: 'id',
        viewrecords: true,
        sortorder: "desc",
        caption:"App users",
        editurl:'/_mijkweb/auth/admin/grid',
        onSelectRow: function(ids) {
            if(ids == null) {
            }else{
                var postdata = jQuery("#list2_d").jqGrid('getGridParam', 'postData');
                jQuery.extend(postdata, {id : ids, stype : 'lalala'});
                jQuery("#list2_d").jqGrid('setGridParam',
                    {url:"/_mijkweb/auth/admin/appuserdetails",
                     postData: postdata,
                     page:1});
                jQuery("#list2_d").jqGrid('setCaption',"App user instances for appid = : "+ids).trigger('reloadGrid');
                $("#list2_d").jqGrid('setGridState', 'visible');
            }
        },
        ondblClickRow: function(rowid) {
            jQuery(this).jqGrid('editGridRow', rowid,
                    {recreateForm:true,closeAfterEdit:true,afterSubmit:jqg_edit_aftersubmit,
                     closeOnEscape:true,reloadAfterSubmit:true})},
        loadComplete: function(data){
            $.each(data.rows,function(i,item){
                if(data.rows[i].cell[1] == 0){
                    $("#" + data.rows[i].id).find("td").eq(1).css("color", "red");
                }
            })},
        onHeaderClick: function(){
            if($("#list2").jqGrid('getGridParam', 'gridstate') == 'visible'){
                $("#list2_d").jqGrid('setGridState', 'visible');
            }else{
                $("#list2_d").jqGrid('setGridState', 'hidden');
            }
        }
    });
    //jQuery("#list2").jqGrid('navGrid','#pager2',{edit:false,add:false,del:false});
    jQuery("#list2").jqGrid('navGrid','#pager2',
        {}, //options
        {height:280,reloadAfterSubmit:false,afterSubmit:jqg_edit_aftersubmit,closeAfterEdit:true}, // edit options
        {height:280,reloadAfterSubmit:true}, // add options
        {reloadAfterSubmit:false}, // del options
        {} // search options
    );
    //jQuery(".ui-jqgrid-bdiv").css('height', 470);
    jQuery("#list2").jqGrid('bindKeys',{scrollingRows: true, onEnter:edit_dialog});

    jQuery("#list2_d").jqGrid({
        mtype: 'POST',
        datatype: "json",
        postData: {type:'select'}, // setGridParam to change dynamically
        height: 100,
        url:'/_mijkweb/auth/admin/appuserdetails',
        datatype: "json",
        colNames:['ID','UserID', 'App Type', 'Device ID'],
        colModel:[
            {name:'id',index:'id', width:55},
            {name:'appuserid',index:'appuserid', width:65},
            {name:'apptype',index:'apptype', width:110, align:"right"},
            {name:'deviceid',index:'deviceid', width:240, align:"right"},        
        ],
        rowNum:5,
        rowList:[5,10,20],
        pager: '#pager2_d',
        sortname: 'id',
        viewrecords: true,
        sortorder: "asc",
        multiselect: true,
        caption:"App user instances"
    }).navGrid('#pager10_d',{add:false,edit:false,del:false});

    $("#list2_d").jqGrid('setGridState', 'hidden');

}

function check_field(value, colname){
    switch(colname){
        case "Login":
            return [true, ""];
        case "Password":
            if(value.length < 8){
                return [false, "Password length should be more or equal 8 symbols"]
            }
            return [true, ""];    
        default:
            return [false, "Davai dosvidania!"];    
    }
}

function jqg_edit_aftersubmit(response, postdata){
    if(response.status == 200){
        var obj = JSON.parse(response.responseText);
        if(obj && obj.status && obj.status == 'ok'){
            return [true, "", null]
        }else{
            return [false, "update failed", null]
        }
    }else{
        return [true, "unknown error", null]    
    }
}

function active_cell_formatter(value, options, cell){
    return value==1?'Yes':'No';
}

function edit_dialog(rowid){
    jQuery(this).jqGrid('editGridRow', rowid,
            {recreateForm:true,closeAfterEdit:true,afterSubmit:jqg_edit_aftersubmit,
             closeOnEscape:true,reloadAfterSubmit:true})
}

function add_event_log(EString){
    var ev      = $("#event_log"); 
    var evlog   = ev.html();
    var ec      = ev.children().length;
    ev.html(evlog + "\n" + '<p class="event_str">' + PRCounter + " - " + EString  + "</p>");
    ev.scrollTop(ev.height());
   
    if(ec > 1000){ $("#event_log p:first").remove(); }
}

function event_l_cw(){
    var ev      = $("#event_log"); 
    ev.empty();
}

function message_tch_box(){
    $("#message_box").dialog("open");
}

function form_submit() {
    var robj = {
        type: 'push',
        message: $("#message_f").val() || "",
        to: $("#to_f").val() || ""
    };
    if($("#to_f_h").val()){
        robj.channel = "channel_" + $("#to_f_h").val()
    };
    $.ajax({
        type: 'POST',
        url:  '/_mijkweb/auth/push',
        beforeSend: function(xhr){
            return true
        },
        data: "request=" + JSON.stringify(robj), 
        success: function(data){
            var obj = JSON.parse(data);
            if(obj.status == 'ok'){
                $('#message_f').val('');
            }else{
            }
        },
        error: function(data){alert('ERROR!'); console.log(data)}
    });
}

function channel_autocomplete(request, response){
    $.ajax({
        type: 'POST',
        url:  '/_mijkweb/auth/admin/user',
        beforeSend: function(xhr){
            return true
        },
        data: "request=" + JSON.stringify({
                type: 'channels-list',
                str:  request.term
            }), 
        success: function(data){
            var obj = JSON.parse(data);
            if(obj.status == 'ok'){
                console.log(obj);
                response($.map(obj.data, function(val, i){ return {label: val[1], value:val[0]}; }));
            }else{ 

            }
        },
        error: function(data){alert('ERROR!'); console.log(data)}
    });
}

function channels_list(Pattern, Limit, SuccessCallBack){
    var obj = {
                type: 'channels-list',
                str:  Pattern,
                limit: Limit
            };
    $.ajax({
        type: 'POST',
        url:  '/_mijkweb/auth/admin/user',
        data: "request=" + JSON.stringify({
                type: 'channels-list',
                str:  Pattern,
                limit: Limit
            }), 
        success: SuccessCallBack,
        error: function(data){alert('ERROR!'); console.log(data)}
    });
}

function move_to_toobox(Id, Label) {
    $(Id).dialog('close');
    var apdiv_id = Id.substring(1)+'_tb';
    var apdiv = $('<div style="" class="status_line tb_item" id="'+apdiv_id+'">'+Label+'</div>');
    $("#mijktoolbar").append(apdiv);
    $('#'+apdiv_id).click(function(){
        $(Id).dialog('open');
        $('#'+apdiv_id).remove();
    });
    return false;
}

function settings_dialog(){
    $("#settings_box").dialog({ 
        collapseEnabled: true,
        autoOpen: false,
        width:'auto',
        open: function(event, ui){ $("#settings_box_tb").remove()},
        beforeCollapse: function(event, ui){ return move_to_toobox('#settings_box', 'Settings'); }
    });
    $("#save_channels").click(function(){
       var ListeningChannelsTmp = ListeningChannels;
       ListeningChannels = [];
       ListeningChannels.push(['channel_'+SysAccountId, 'Common']);
       $.each($('#sortable2 li'), function(i, val){
           var un = $(val).text();
           Channel = get_channel_mkh(UserChannels, ListeningChannelsTmp, un)
           console.log(un, UserChannels[un], Channel);
           if((un != 'Common') && Channel){
                ListeningChannels.push(['channel_'+Channel, un]);
           }
        });
       $("#settings_box").dialog('close'); 
    });
    $("#cancel_channels").click(function(){
       $("#settings_box").dialog('close'); 
    });
}

function get_channel_mkh(UC, LCH, un){
    if(UC[un]){
        return UC[un]
    }else{
       for(var i=0;i<LCH.length;i++){
           if(LCH[i][1] == un){ return LCH[i][0]}
       }         
    }
    return false;
}

function settings_tch_box(){
    $("#settings_box").dialog("open");
    // have to remove all childrens from #sortable1 & 2
    $('#sortable1 li').remove();
    $('#sortable2 li').remove();
    channels_list("", 100, function(data){
        var obj = JSON.parse(data);
        if(obj.status == 'ok'){
            UserChannels = [];
            $.each(obj.data, function(i, val){
                if(! already_listening(ListeningChannels, val[0]) ){
                    UserChannels[val[1]] = val[0];
                    var liIt = $('<li class="ui-state-default">'+val[1]+'</li>'); 
                    $("#sortable1").append(liIt);
                }
            });
            $( "#sortable1, #sortable2" ).sortable({
                  connectWith: ".connectedSortable"
                }).disableSelection();
        }else{ 

        }
    });
    $.each(ListeningChannels, function(i, val){
        var liIt = $('<li class="ui-state-default">'+val[1]+'</li>'); 
        $("#sortable2").append(liIt);
    });
}

function already_listening(ListeningChannelsTmp, channel){
    var ch = 'channel_'+channel;
    for(var i=0; i < ListeningChannelsTmp.length; i++){
        if(ch == ListeningChannelsTmp[i][0]){return 1}
    }
    return 0
}

function get_stat_mkh(){
    $.ajax({
        type: 'POST',
        url:  '/_mijkweb/auth/admin/user',
        beforeSend: function(xhr){
            return true
        },
        data: "request=" + JSON.stringify({type: 'get-admin-stats'}), 
        success: function(data){
            var obj = JSON.parse(data);
            if(obj.status == 'ok'){
                $('#stat_mkh_online').text(obj.data.online);
                $('#stat_mkh_reg_today').text(obj.data.reg_today);
                $('#stat_mkh_logins_today').text(obj.data.logins_today);
                $('#stat_mkh_events_today').text(obj.data.events_today);
                $('#stat_mkh_key_using').text(obj.data.key_using);
            }else{ 

            }
        },
        error: function(data){console.log(data)}
    });
}

function admin_logout(){
   $.removeCookie("MIJKSSID");
   window.location = '/';    
}

function update_top_stats(obj){
    if(obj.data && obj.data.online_stats){
        $('#stat_mkh_online').text(obj.data.online_stats.online);
        $('#stat_mkh_reg_today').text(obj.data.online_stats.reg_today);
        $('#stat_mkh_logins_today').text(obj.data.online_stats.logins_today);
        $('#stat_mkh_events_today').text(obj.data.online_stats.events_today);
        $('#stat_mkh_key_using').text(obj.data.online_stats.key_using);
    }    
}
