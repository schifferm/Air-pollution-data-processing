<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
</head>
<body>

<h1>Air-pollution-data-processing</h1>

<h2>Introduction</h2>

> 空氣是我們的皮膚最常接觸到的物件，而空氣中夾雜著有害物質與汙染物，長期下來會給予我們什麼負面影響與疾病？
  這個範例結合了高雄榮總醫院皮膚科與免疫科的每日就診人數與高雄地區空氣觀測資料來做初步的分析。


<h2>Data source</h2>

><p>行政院環保署<a href="https://taqm.epa.gov.tw/taqm/tw/YearlyDataDownload.aspx">106年高屏空品區全年逐時資料</a></p>

<h2>Data information</h2>

>此資料分為

<p><ol>
<li>每日就診人數</li>
	日期為106年1月1日到12月31日，ICD代碼為703、995.3的就診病人。
<li>空氣品質監測資料</li>
	這裡我們列出感興趣的檢測項目，如下：
	<ol>
		<li><code>SO<sub>2</sub></code></li>
		<li><code>CO</code></li>
		<li><code>O<sub>3</sub></code></li>
		<li><code>PM<sub>10</sub></code></li>
		<li><code>PM<sub>2.5</sub></code></li>
		<li><code>NO</code></li>
		<li><code>NO<sub>2</sub></code></li>
	</ol>
	每個檢測項目都有全天候24小時逐時資料，但還是有少許遺漏值。	
</ol></p>

<h2>Pretreatment</h2>

<p>to be updated</p>

<h2>Statistics Graph</h2>
<h3>O3</h3>
<img src="https://github.com/schifferm/Air-pollution-data-processing/blob/master/O3_date.png" alt="O3_date">
<img src="https://github.com/schifferm/Air-pollution-data-processing/blob/master/O3_patient.png" alt="O3_patient">
<h3>PM2.5</h3>
<img src="https://github.com/schifferm/Air-pollution-data-processing/blob/master/PM2.5_date.png" alt="PM2.5_date">
<img src="https://github.com/schifferm/Air-pollution-data-processing/blob/master/PM2.5_patient.png" alt="PM2.5_patient">
<h2>Loess Regression graph </h2>
<h3>O3</h3>
<h4>一個禮拜的空氣汙染指標對就診人數的預測</h4>
<img src="https://github.com/schifferm/Air-pollution-data-processing/blob/master/loess_O3_weeks.png" alt="loess_O3_weeks">
<h4>上個禮拜的空氣汙染指標對這個禮拜的就診人數預測</h4>
<img src="https://github.com/schifferm/Air-pollution-data-processing/blob/master/loess_O3_lastweek.png" alt="loess_O3_lastweek">
<h3>PM2.5</h3>
<h4>一個禮拜的空氣汙染指標對就診人數的預測</h4>
<img src="https://github.com/schifferm/Air-pollution-data-processing/blob/master/loess_PM2.5_weeks.png" alt="loess_PM2.5_weeks">
<h4>上個禮拜的空氣汙染指標對這個禮拜的就診人數預測</h4>
<img src="https://github.com/schifferm/Air-pollution-data-processing/blob/master/loess_PM2.5_lastweek.png" alt="loess_PM2.5_lastweek">

</body>
</html>