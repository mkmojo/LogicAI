
var query_results
    
var $ = function(id){
     return document.getElementById(id);
}

var request = null, session = null

function postEscape(val) {
     return encodeURIComponent(val).replace(/%20/g, "+")
}

function update_display(){
     content = ""
     for(var i = 0; i < query_results.length; i++){
          line = query_results[i];
          if(i == 0){
               content += line;
          }
          else{
               content += '\n' + line;
          }
     }
     $("display").value = content
     $("display").scrollTop = $("display").scrollHeight;
}


function respond_send(){
     if( request.readyState != 4) return  
//     alert(request.responseText)
     query_results = eval(request.responseText)
     update_display()
}


function submit_send() {
     var new_text = $("input_text").value;
     var time = new Date().getTime().toString()
//     alert(typeof(time))
     request = new XMLHttpRequest()
     request.onreadystatechange = respond_send
     request.open("POST", "index.cgi", true  )
     request.setRequestHeader("Content-type","application/x-www-form-urlencoded")
     request.send( "action=send"+
     			"&content=" + postEscape(new_text) +
     			"&session=" + postEscape(session) +
     			"&time=" + postEscape(time))

     document.form.message.value = ""
}


function respond_login() {
	if( request.readyState != 4) return
     	session = request.responseText
     	session = session.replace(/\s/g, '')
     	//used to get rid of newlines
     	if (session == "failed"){
		alert("Incorrect Password/Username")
		session = null
     	}
     	else{
     		document.form.connected.checked = true
     	}
}

function submit_login() {
	if( document.form.connected.checked ) return

	setInterval(function(){query()},1000);
	var time = new Date().getTime().toString()
	request = new XMLHttpRequest()
	request.onreadystatechange = respond_login
	request.open("POST", "index.cgi", true  )
	request.setRequestHeader("Content-type","application/x-www-form-urlencoded")
	request.send( "action=login" +
			"&password=" + postEscape( document.form.password.value ) +
			"&username=" + postEscape( document.form.username.value ) +
			"&session=" + postEscape( session ) +
			"&time_stamp=" + postEscape(time) )
	document.form.password.value = ""
	document.form.username.value = ""

}

function submit_logout() {
	var form = document.form
	if( !form.connected.checked ) return
	session = null
	query_results = null
	form.connected.checked = false
	$("display").value = "You have logged out"
	//do AJAX submission to get rid of session server side
}


function respond_query(){
	if( request.readyState != 4) return
	//alert(request.responseText)
	query_results = eval(request.responseText)
     	update_display()
}


function query()
{
	var time = new Date().getTime().toString()
	request = new XMLHttpRequest()
	request.onreadystatechange = respond_query
	request.open("POST", "index.cgi", true  )
	request.setRequestHeader("Content-type","application/x-www-form-urlencoded")
	request.send( "action=query" +
			"&session=" + postEscape( session ) +
			"&time_stamp=" + postEscape(time) )
}



