<h1 align="center">Inflation Dynamics in Pakistan: A Statistical Analysis</h1>
<p>
  Advanced Statistics — Final Project<br>
  <strong>Authors:</strong> Hussain Ibrahim, Mahd Kazmi, Sameer Khan
</p>
<h2>Overview</h2>
<p>
  This repository studies Pakistan’s inflation using annual macroeconomic data (1980–2023) and generates a 2024 forecast. 
  Variables considered include Broad Money growth, Food Imports, GDP growth, Exchange Rate, Crude Oil, and REER. 
</p>
<h2>Key Findings</h2>
<ul>
  <li>Coverage: 1980–2023; 2024 forecast included</li>
  <li>Model comparison shows <strong>ARIMA</strong> achieved the lowest test MSE among candidates</li>
  <li>Forecasted inflation for 2024: <strong>30.77%</strong></li>
  <li>Important channels discussed: monetary expansion (M2), exchange-rate pass-through, food/imported inflation, and oil price shocks</li>
</ul>

<h2>Files</h2>
<ul>
  <li><code>Inflation_Dataset_v1.csv</code> — initial cleaned dataset</li>
  <li><code>Inflation_Dataset_v2.csv</code> — updated dataset</li>
  <li><code>ER.csv</code> — nominal exchange rate (PKR per USD)</li>
  <li><code>REER.csv</code> — real effective exchange rate index (2010 = 100)</li>
  <li><code>CMO-Historical-Data-Annual.xlsx</code> — supplementary indicators</li>
  <li><code>Analysis0.R</code>, <code>Analysis1_py.ipynb</code>, <code>Analysis2_py.ipynb</code> — analysis scripts/notebooks</li>
  <li><code>Inflation Dynamics in Pakistan.pdf</code> — full report (methods, diagnostics, tables)</li>
</ul>

<h2>Visualisations of Results & Data</h2>
<p><strong>Actual vs Predicted Inflation</strong></p>
<p><img src="pictures/Actual vs Predicted Inflation.png" alt="Actual vs Predicted Inflation" /></p>
<hr />
<p><strong>Correlation Matrix of Economic Variables</strong></p>
<p><img src="pictures/Corerelation Matrix of Economic Variables.png" alt="Correlation Matrix of Economic Variables" /></p>
<hr />
<p><strong>Distribution of Economic Variables</strong></p>
<p><img src="pictures/Distribution of Economic Variables.png" alt="Distribution of Economic Variables" /></p>
<hr />
<p><strong>Scatterplot Matrix of Economic Variables</strong></p>
<p><img src="pictures/Scatterplot Matrix of Economic Variables.png" alt="Scatterplot Matrix of Economic Variables" /></p>
<hr />
<p><strong>Time Series Plots</strong></p>
<p><img src="pictures/Time Series Plots.png" alt="Time Series Plots" /></p>
<hr />
<p><strong>Inflation vs Variables</strong></p>
<p><img src="pictures/Inflation vs Variabes.png" alt="Inflation vs Variables" /></p>
<hr />
<p><strong>Historcal Inflation & Forecast</strong></p>
<p><img src="pictures/Historcal Inflation & Forecast.png" alt="Historical Inflation &amp; Forecast" /></p>
<hr />
<p><strong>Model Comparison</strong></p>
<p><img src="pictures/Model Comparison.png" alt="Model Comparison" /></p>
<hr />
<p><strong>MSE Comparison</strong></p>
<p><img src="pictures/MSE Comparison.png" alt="MSE Comparison" /></p>
<hr />
<p><strong>Prediction Accuracy</strong></p>
<p><img src="pictures/Prediction Accuracy.png" alt="Prediction Accuracy" /></p>
<hr />
<p><strong>Variable Importance by Model</strong></p>
<p><img src="pictures/Variable Importance by Model.png" alt="Variable Importance by Model" /></p>
<hr />


<h2>Methods</h2>
<ul>
  <li><strong>AR, MA, ARMA, ARIMA</strong>: time-series models for inflation; best AIC found for ARIMA(0,0,0) in selection table</li>
  <li><strong>Regularised regressions</strong>: Ridge, LASSO, Elastic Net fitted and compared with test MSE against ARIMA</li>
</ul>

<h2>Report</h2>
<p>
  For detailed background, data description, model selection tables (AIC/MSE), and interpretation, see 
  <a href="Inflation Dynamics in Pakistan.pdf">Inflation Dynamics in Pakistan</a>
</p>
