google.load("visualization", "1", {packages:["corechart"]});
//google.setOnLoadCallback(drawChart);
function drawChart(dataArray) {
        var data = google.visualization.arrayToDataTable(dataArray);

        var options = {
          title: 'My Daily Activities',
          isStacked:true
        };

        var chart = new google.visualization.BarChart(document.getElementById('chart_div'));
        chart.draw(data, options);
}