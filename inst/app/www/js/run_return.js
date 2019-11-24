$(document).keydown(function(event) {
  if ($("#cor_name").is(":focus") && event.keyCode == 13) {
    $("#cor_store").click();
  }
});
