let data = Highcharts.geojson(Highcharts.maps['countries/us/us-all']);
const separators = Highcharts.geojson(Highcharts.maps['countries/us/us-all'], 'mapline');
let mapSelection = "United States";
let varSelection = 'renting';

let instructions = document.getElementById('instructions');
let renting = document.getElementById('rentingButton');
let mortgage = document.getElementById('mortgageButton');
let buttonBox = document.getElementsByClassName('highcharts-button-box');

let housingDataState = dl.csv('https://raw.githubusercontent.com/matthew-martinez/R/main/housingcosts/BurdenStates.csv');
let housingDataCounty = dl.csv('https://raw.githubusercontent.com/matthew-martinez/R/main/housingcosts/BurdenCounties.csv');
let housingDataStateYearly = dl.csv('https://raw.githubusercontent.com/matthew-martinez/R/main/housingcosts/BurdenYearly.csv');

// function to select specific state to only map that single state
let USTimeDataOwners = [37.08, 37.66, 37.73, 37.65, 38.02, 36.85, 33.9, 31.74, 30.83, 29.56, 28.29, 27.50, 27.70, 26.61];
let USTimeDataRenters = [49.77, 49.31, 49.78, 51.54, 52.98, 53.41, 52.01, 51.53, 51.77, 50.59, 49.73, 49.54, 49.69, 48.43];

let processedTimeDataOwners = [];
let processedTimeDataRenters = [];

function getYearly(state) {
    for (i = 0; i < housingDataStateYearly.length; i++) {  
        if (housingDataStateYearly[i].State === state){
            processedTimeDataOwners.push(housingDataStateYearly[i].ownBurdenPerc);
            processedTimeDataRenters.push(housingDataStateYearly[i].rentBurdenPerc);
        }
    }
}

function zeroPad(num) {
    return num.toString().padStart(5, "0");
  }
  
for (i = 0; i < housingDataCounty.length; i++) {      
    housingDataCounty[i].GEOID = zeroPad(housingDataCounty[i].GEOID);
}
// Line chart for time trends
let line = Highcharts.chart('line', {

    title: {
      text: 'Cost-burdened Households in The United States (Percent)'
    },
  
    subtitle: {
      text: ''
    },
  
    yAxis: {
      title: {
        text: ''
      },
      tickInterval: 10,
      min: 16,
      max: 65
    },
    xAxis: {
        title: {
            text: ''
        },
        tickInterval: 2,
        min: 2006,
        max: 2019
    },
    legend: {
      layout: 'horizontal',
      align: 'bottom',
      verticalAlign: 'bottom'
    },
  
    plotOptions: {
      series: {
        label: {
          connectorAllowed: false
        },
        pointStart: 2006,
        marker: {
            enabled: false
        }
      }
    },
  
    responsive: {
      rules: [{
        condition: {
          maxWidth: 500
        },
        chartOptions: {
          legend: {
            layout: 'horizontal',
            align: 'center',
            verticalAlign: 'bottom'
          }
        }
      }]
    },
    series: [{
        name: "Owning with Mortgage",
        data: USTimeDataOwners,
        color: '#084081'
    },
    {
        name: "Renting",
        data: USTimeDataRenters,
        color: '#7bccc4'
    }]
  });

function lineUpdate() {
    mapSelection = "United States"; // change to US when new data is in
        let statement = `Cost-burdened Households in ${mapSelection} (Percent)`;
        let USTimeDataOwners = [37.08, 37.66, 37.73, 37.65, 38.02, 36.85, 33.9, 31.74, 30.83, 29.56, 28.29, 27.50, 27.70, 26.61];
        let USTimeDataRenters = [49.77, 49.31, 49.78, 51.54, 52.98, 53.41, 52.01, 51.53, 51.77, 50.59, 49.73, 49.54, 49.69, 48.43];
        line.update({
            series: [{
                name: "Owning with Mortgage",
                data: USTimeDataOwners,
                color: '#084081'
            },
            {
                name: "Renting",
                data: USTimeDataRenters,
                color: '#7bccc4'
            }],
            title: {
                text: statement
            },
            yAxis: {
                title: {
                    text: ''
                },
                tickInterval: 10,
                min: 16,
                max: 65
            },
            xAxis: {
                title: {
                    text: ''
                },
                tickInterval: 2,
                min: 2006,
                max: 2019
            }
    })
}

// Set drilldown pointers
data.forEach((d, i) => {
  d.drilldown = d.properties['hc-key'];
  d.value = i;
});

for (i = 0; i < data.length; i++){
  for (j = 0; j < housingDataState.length; j++){
      if (data[i].name === housingDataState[j].State) {
          data[i].value = housingDataState[j].rentBurdenPerc; // change to column that you want to display on the map
      }
  }
}

function cleaner(varSelection){
    if (varSelection === 'renting'){
        for (i = 0; i < data.length; i++){
            for (j = 0; j < housingDataState.length; j++){
                if (data[i].name === housingDataState[j].State) {
                    data[i].value = housingDataState[j].rentBurdenPerc; // change to column that you want to display on the map
                }
            }
        }
    }

    if (varSelection === 'mortgage'){
        for (i = 0; i < data.length; i++){
            for (j = 0; j < housingDataState.length; j++){
                if (data[i].name === housingDataState[j].State) {
                    data[i].value = housingDataState[j].ownBurdenPerc; // change to column that you want to display on the map
                }
            }
        }
    }
    return data;
}

// modify value with the column of choice from housingDataState
function getScript(url, cb) {
    const script = document.createElement('script');
    script.src = url;
    script.onload = cb;
    document.head.appendChild(script);
}

let rentProps = {
    chart: {
        events: {
            drilldown: function (e) {
                if (!e.seriesOptions) {
                    const chart = this,
                        mapKey = 'countries/us/' + e.point.drilldown + '-all';

                    // Handle error, the timeout is cleared on success
                    let fail = setTimeout(() => {
                        if (!Highcharts.maps[mapKey]) {
                            chart.showLoading('<i class="icon-frown"></i> Failed loading ' + e.point.name);
                            fail = setTimeout(() => {
                                chart.hideLoading();
                            }, 1000);
                        }
                    }, 3000);

                    // Show the spinner
                    chart.showLoading('<i class="icon-spinner icon-spin icon-3x"></i>'); // Font Awesome spinner

                    // Load the drilldown map
                    getScript('https://code.highcharts.com/mapdata/' + mapKey + '.js', () => {
                        data2 = Highcharts.geojson(Highcharts.maps[mapKey]);

                        // Set a non-random bogus value - modify value with county data from housingDataCounty
                        data2.forEach((d, i) => {
                            d.value = i;
                        });
                            for (i = 0; i < data2.length; i++){
                                for (j = 0; j < housingDataCounty.length; j++){
                                    if (data2[i].properties.fips === housingDataCounty[j].GEOID) {
                                        data2[i].value = housingDataCounty[j].rentBurdenPerc; // change to column that you want to display on the map
                                    }
                                }
                        }
                        // Hide loading and add series
                        chart.hideLoading();
                        clearTimeout(fail);
                        chart.addSeriesAsDrilldown(e.point, {
                            name: e.point.name,
                            data: data2,
                            dataLabels: {
                                enabled: false,
                                format: '{point.name}'
                            }
                        });
                    });
                }

                this.setTitle(null, { text: "Renting" });
                mapSelection = e.point.name;
                //console.log(mapSelection)
                processedTimeDataOwners = []
                processedTimeDataRenters = []
                getYearly(mapSelection)
                let statement = `Cost-burdened Households in ${mapSelection} (Percent)`;
                line.update({
                    series: [{
                        name: "Owning with Mortgage",
                        data: processedTimeDataOwners,
                        color: '#084081'
                    },
                    {
                        name: "Renting",
                        data: processedTimeDataRenters,
                        color: '#7bccc4'
                    }],
                        title: {
                            text: statement
                        },
                        yAxis: {
                            title: {
                                text: ''
                            },
                            tickInterval: 10,
                            min: 16,
                            max: 65
                        }
                    })
            },


            drillup: function () {
                this.setTitle(null, { text: 'Renting' });
                mapSelection = "United States"; // change to US when new data is in
                let statement = `Cost-burdened Households in ${mapSelection} (Percent)`;
                let USTimeDataOwners = [37.08, 37.66, 37.73, 37.65, 38.02, 36.85, 33.9, 31.74, 30.83, 29.56, 28.29, 27.50, 27.70, 26.61];
                let USTimeDataRenters = [49.77, 49.31, 49.78, 51.54, 52.98, 53.41, 52.01, 51.53, 51.77, 50.59, 49.73, 49.54, 49.69, 48.43];
                line.update({
                    series: [{
                        name: "Owning with Mortgage",
                        data: USTimeDataOwners,
                        color: '#084081'
                    },
                    {
                        name: "Renting",
                        data: USTimeDataRenters,
                        color: '#7bccc4'
                    }],
                    title: {
                        text: statement
                    },
                    yAxis: {
                        title: {
                            text: ''
                        }
                    }
                })
            }
        }
    },

    title: {
        text: 'Cost-burdened Households (Percent)'
    },

    subtitle: {
        text: 'Renting'
    },

    colorAxis: {
        dataClasses: [{
            to: 38.2,
            color: '#ccebc5',
            name: "< 38.2%"
        }, {
            from: 38.2,
            to: 44.4,
            color: '#7bccc4',
            name: "38.2% - 44.4%"
        }, {
            from: 44.4,
            to: 49.6,
            color: '#2b8cbe',
            name: "44.4% - 49.6%"
        }, {
            from: 49.6,
            color: '#084081',
            name: "> 49.6%"
        }, {
            from: "NA",
            to: "NA",
            name: "No Data",
            color: "000000"
        }]
    },

    mapNavigation: {
        enabled: false,
        buttonOptions: {
            verticalAlign: 'bottom'
        }
    },

    plotOptions: {
        map: {
            states: {
                hover: {
                    borderColor: '#ffffff'
                }
            }
        }
    },

    legend: {
            layout: 'horizontal',
            align: 'center',
            verticalAlign: 'bottom',
            title: {
                text: ""
            }
          },

    series: [{
        data: data,
        name: 'U.S.',
        dataLabels: {
            enabled: false,
            format: '{point.properties.postal-code}'
        }
    }, {
        type: 'mapline',
        data: separators,
        color: 'silver',
        enableMouseTracking: false,
        animation: {
            duration: 200
        }
    }],

    drilldown: {
        activeDataLabelStyle: {
            color: '#FFFFFF',
            textDecoration: 'none',
            textOutline: '1px #000000'
        },
        drillUpButton: {
            relativeTo: 'spacingBox',
            position: {
                x: -15,
                y: 33
            }
        }
    }
}

let mortgageProps = {
    chart: {
        events: {
            drilldown: function (e) {
                if (!e.seriesOptions) {
                    const chart = this,
                        mapKey = 'countries/us/' + e.point.drilldown + '-all';

                    // Handle error, the timeout is cleared on success
                    let fail = setTimeout(() => {
                        if (!Highcharts.maps[mapKey]) {
                            chart.showLoading('<i class="icon-frown"></i> Failed loading ' + e.point.name);
                            fail = setTimeout(() => {
                                chart.hideLoading();
                            }, 1000);
                        }
                    }, 3000);

                    // Show the spinner
                    chart.showLoading('<i class="icon-spinner icon-spin icon-3x"></i>'); // Font Awesome spinner

                    // Load the drilldown map
                    getScript('https://code.highcharts.com/mapdata/' + mapKey + '.js', () => {
                        data2 = Highcharts.geojson(Highcharts.maps[mapKey]);

                        // Set a non-random bogus value - modify value with county data from housingDataCounty
                        data2.forEach((d, i) => {
                            d.value = i;
                        });
                            for (i = 0; i < data2.length; i++){
                                for (j = 0; j < housingDataCounty.length; j++){
                                    if (data2[i].properties.fips === housingDataCounty[j].GEOID) {
                                        data2[i].value = housingDataCounty[j].ownBurdenPerc; // change to column that you want to display on the map
                                    }
                                }
                        }
                        // Hide loading and add series
                        chart.hideLoading();
                        clearTimeout(fail);
                        chart.addSeriesAsDrilldown(e.point, {
                            name: e.point.name,
                            data: data2,
                            dataLabels: {
                                enabled: false,
                                format: '{point.name}'
                            }
              });
                    });
                }

                this.setTitle(null, { text: "Owning with Mortgage" });
                mapSelection = e.point.name;
                //console.log(mapSelection)

                processedTimeDataOwners = []
                processedTimeDataRenters = []
                getYearly(mapSelection)
                //console.log(processedTimeDataOwners)
                let statement = `Cost-burdened Households in ${mapSelection} (Percent)`;
                line.update({
                    series: [{
                        name: "Owning with Mortgage",
                        data: processedTimeDataOwners,
                        color: '#084081'
                    },
                    {
                        name: "Renting",
                        data: processedTimeDataRenters,
                        color: '#7bccc4'
                    }],
                        title: {
                            text: statement
                        },
                        yAxis: {
                            title: {
                                text: ''
                            },
                            tickInterval: 10,
                            min: 16,
                            max: 65
                    }
                })
            },


            drillup: function () {
                this.setTitle(null, { text: 'Owning with Mortgage' });
                mapSelection = "United States"; // change to US when new data is in
                let statement = `Cost-burdened Households in ${mapSelection} (Percent)`;
                let USTimeDataOwners = [37.08, 37.66, 37.73, 37.65, 38.02, 36.85, 33.9, 31.74, 30.83, 29.56, 28.29, 27.50, 27.70, 26.61];
                let USTimeDataRenters = [49.77, 49.31, 49.78, 51.54, 52.98, 53.41, 52.01, 51.53, 51.77, 50.59, 49.73, 49.54, 49.69, 48.43];
                line.update({
                    series: [{
                        name: "Owning with Mortgage",
                        data: USTimeDataOwners,
                        color: '#084081'
                    },
                    {
                        name: "Renting",
                        data: USTimeDataRenters,
                        color: '#7bccc4'
                    }],
                    title: {
                        text: statement
                    },
                    yAxis: {
                        title: {
                            text: ''
                        },
                        tickInterval: 10,
                        min: 16,
                        max: 68
                    }
                })
            }
        }
    },

    title: {
        text: 'Cost-burdened Households (Percent)'
    },

    subtitle: {
        text: 'Owning with Mortgage'
    },

    colorAxis: {
        dataClasses: [{
            to: 19.3,
            color: '#ccebc5',
            name: "< 19.3%"
        }, {
            from: 19.3,
            to: 20.3,
            color: '#7bccc4',
            name: "19.3% - 20.3%"
        }, {
            from: 20.3,
            to: 27,
            color: '#2b8cbe',
            name: "20.3% - 24.0%"
        }, {
            from: 27,
            color: '#084081',
            name: "> 27%"
        }, {
        from: "NA",
        to: "NA",
        name: "No Data",
        color: "000000"
        }]
    },

    mapNavigation: {
        enabled: false,
        buttonOptions: {
            verticalAlign: 'bottom'
        }
    },

    plotOptions: {
        map: {
            states: {
                hover: {
                    borderColor: '#ffffff'
                }
            }
        }
    },

    series: [{
        data: data,
        name: 'U.S.',
        dataLabels: {
            enabled: false,
            format: '{point.properties.postal-code}'
        }
    }, {
        type: 'mapline',
        data: separators,
        color: 'silver',
        enableMouseTracking: false,
        animation: {
            duration: 200
        }
    }],

    drilldown: {
        activeDataLabelStyle: {
            color: '#FFFFFF',
            textDecoration: 'none',
            textOutline: '1px #000000'
        },
        drillUpButton: {
            relativeTo: 'spacingBox',
            position: {
                x: -15,
                y: 33
            }
        }
    }
  }

let housingMap = new Highcharts.mapChart('map', rentProps)
document.addEventListener('DOMContentLoaded', housingMap);

function mapper(chartType) {
  let housingMap = new Highcharts.mapChart('map', chartType);
}

mortgage.addEventListener('click', function() {
  mortgageButton.style = "background-color: #084081; color: white";
  rentingButton.style = "background-color: white; color: black";
  //rentingButton.style = "";
  mortgageButton.innerHTML = "Mortgage";
  varSelection='mortgage';

  // Set drilldown pointers
  data.forEach((d, i) => {
    d.drilldown = d.properties['hc-key'];
    d.value = i;
  });

  for (i = 0; i < data.length; i++){
    for (j = 0; j < housingDataState.length; j++){
        if (data[i].name === housingDataState[j].State) {
            data[i].value = housingDataState[j].ownBurdenPerc; // change to column that you want to display on the map
        }
    }
  }

  cleaner('mortgage');
  mapper(mortgageProps);
  lineUpdate();
})

renting.addEventListener('click', function() {
  rentingButton.style = "background-color: #084081; color: white";
  mortgageButton.style = "background-color: white; color: black";
  mortgageButton.innerHTML = "Mortgage";
  varSelection='renting';

  // Set drilldown pointers
data.forEach((d, i) => {
  d.drilldown = d.properties['hc-key'];
  d.value = i;
});

for (i = 0; i < data.length; i++){
  for (j = 0; j < housingDataState.length; j++){
      if (data[i].name === housingDataState[j].State) {
          data[i].value = housingDataState[j].rentBurdenPerc; // change to column that you want to display on the map
      }
  }
}

  cleaner('renting');
  mapper(rentProps);
  lineUpdate();
})
