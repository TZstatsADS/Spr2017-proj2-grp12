# Get this figure: fig <- get_figure("RhettAllain", 111)
# Get this figure's data: data <- get_figure("RhettAllain", 111)$data
# Add data to this figure: p <- add_trace(p, x=c(4, 5), y=c(4, 5), kwargs=list(filename="flashPlot", fileopt="extend"))
# Get y data of first trace: y1 <- get_figure("RhettAllain", 111)$data[[1]]$y

# Get figure documentation: https://plot.ly/r/get-requests/
# Add data documentation: https://plot.ly/r/file-options/

# You can reproduce this figure in R with the following code!

# Learn about API authentication here: https://plot.ly/r/getting-started
# Find your api_key here: https://plot.ly/settings/api

library(plotly)
trace1 <- list(
  x = c("0.0", "0.041666666666656964", "0.08333333333331393", "0.125", "0.16666666666665697", "0.20833333333331394", "0.25", "0.29166666666665697", "0.33333333333331394", "0.375", "0.41666666666665697", "0.45833333333331394", "0.5", "0.541666666666657", "0.5833333333333139"), 
  y = c("203.69266657523866", "186.82492843513415", "169.18397529594483", "150.29314425849486", "139.79326638457422", "130.4467260663175", "120.2906721021869", "110.40243246537625", "100.73480842442002", "91.15333996647753", "80.57236947680006", "60.34546617625317", "40.746132336437924", "20.734431467812534", "1.1530456063508012"), 
  mode = "markers", 
  name = "x position [m]", 
  type = "scatter", 
  uid = "0b9334"
)
trace2 <- list(
  x = c(0, 0.0119047619048, 0.0238095238095, 0.0357142857143, 0.047619047619, 0.0595238095238, 0.0714285714286, 0.0833333333333, 0.0952380952381, 0.107142857143, 0.119047619048, 0.130952380952, 0.142857142857, 0.154761904762, 0.166666666667, 0.178571428571, 0.190476190476, 0.202380952381, 0.214285714286, 0.22619047619, 0.238095238095, 0.25, 0.261904761905, 0.27380952381, 0.285714285714, 0.297619047619, 0.309523809524, 0.321428571429, 0.333333333333, 0.345238095238, 0.357142857143, 0.369047619048, 0.380952380952, 0.392857142857, 0.404761904762, 0.416666666667, 0.428571428571, 0.440476190476, 0.452380952381, 0.464285714286, 0.47619047619, 0.488095238095, 0.5, 0.511904761905, 0.52380952381, 0.535714285714, 0.547619047619, 0.559523809524, 0.571428571429, 0.583333333333), 
  y = c(199.393799894, 195.626345218, 191.858890542, 188.091435866, 184.32398119, 180.556526515, 176.789071839, 173.021617163, 169.254162487, 165.486707811, 161.719253135, 157.95179846, 154.184343784, 150.416889108, 146.649434432, 142.881979756, 139.11452508, 135.347070405, 131.579615729, 127.812161053, 124.044706377, 120.277251701, 116.509797025, 112.74234235, 108.974887674, 105.207432998, 101.439978322, 97.6725236463, 93.9050689704, 90.1376142946, 86.3701596188, 82.602704943, 78.8352502671, 75.0677955913, 71.3003409155, 67.5328862396, 63.7654315638, 59.997976888, 56.2305222122, 52.4630675363, 48.6956128605, 44.9281581847, 41.1607035088, 37.393248833, 33.6257941572, 29.8583394813, 26.0908848055, 22.3234301297, 18.5559754539, 14.788520778), 
  line = list(
    color = "rgb(55, 126, 184)", 
    width = 4
  ), 
  name = "x position [m] - fit", 
  opacity = 0.5, 
  type = "scatter", 
  uid = "c607b8", 
  xaxis = "x", 
  yaxis = "y"
)
data <- list(trace1, trace2)
layout <- list(
  annotations = list(
    list(
      x = 0.297619047619, 
      y = 105.207432998, 
      align = "left", 
      arrowcolor = "#636363", 
      arrowhead = 2, 
      arrowsize = 1, 
      arrowwidth = 2, 
      ax = 28, 
      ay = -106, 
      bgcolor = "rgba(0,0,0,0)", 
      bordercolor = "", 
      borderpad = 1, 
      borderwidth = 1, 
      font = list(size = 16), 
      opacity = 0.8, 
      showarrow = TRUE, 
      text = "R<sup>2</sup> = 0.9840<br>y   = 199 - 316x", 
      xanchor = "auto", 
      xref = "x", 
      yanchor = "auto", 
      yref = "y"
    )
  ), 
  autosize = TRUE, 
  bargap = 0.2, 
  bargroupgap = 0, 
  barmode = "group", 
  boxgap = 0.3, 
  boxgroupgap = 0.3, 
  boxmode = "overlay", 
  dragmode = "zoom", 
  font = list(
    color = "#444", 
    family = "\"Open sans\", verdana, arial, sans-serif", 
    size = 12
  ), 
  height = 672, 
  hidesources = FALSE, 
  hovermode = "x", 
  legend = list(
    x = 1.02, 
    y = 1, 
    bgcolor = "#fff", 
    bordercolor = "#444", 
    borderwidth = 0, 
    font = list(
      color = "", 
      family = "", 
      size = 0
    ), 
    traceorder = "normal", 
    xanchor = "left", 
    yanchor = "top"
  ), 
  margin = list(
    r = 80, 
    t = 100, 
    autoexpand = TRUE, 
    b = 80, 
    l = 80, 
    pad = 0
  ), 
  paper_bgcolor = "#fff", 
  plot_bgcolor = "#fff", 
  separators = ".,", 
  showlegend = TRUE, 
  smith = FALSE, 
  title = "Flash Running in NY City", 
  titlefont = list(
    color = "", 
    family = "", 
    size = 0
  ), 
  width = 1189, 
  xaxis = list(
    anchor = "y", 
    autorange = TRUE, 
    autotick = TRUE, 
    domain = c(0, 1), 
    dtick = 0.1, 
    exponentformat = "B", 
    gridcolor = "#eee", 
    gridwidth = 1, 
    linecolor = "#444", 
    linewidth = 1, 
    mirror = FALSE, 
    nticks = 0, 
    overlaying = FALSE, 
    position = 0, 
    range = c(-0.0347290505845, 0.618062383918), 
    rangemode = "normal", 
    showexponent = "all", 
    showgrid = TRUE, 
    showline = FALSE, 
    showticklabels = TRUE, 
    tick0 = 0, 
    tickangle = "auto", 
    tickcolor = "#444", 
    tickfont = list(
      color = "", 
      family = "", 
      size = 0
    ), 
    ticklen = 5, 
    ticks = "", 
    tickwidth = 1, 
    title = "Time [s]", 
    titlefont = list(
      color = "", 
      family = "", 
      size = 0
    ), 
    type = "linear", 
    zeroline = TRUE, 
    zerolinecolor = "#444", 
    zerolinewidth = 1
  ), 
  yaxis = list(
    anchor = "x", 
    autorange = TRUE, 
    autotick = TRUE, 
    domain = c(0, 1), 
    dtick = 50, 
    exponentformat = "B", 
    gridcolor = "#eee", 
    gridwidth = 1, 
    linecolor = "#444", 
    linewidth = 1, 
    mirror = FALSE, 
    nticks = 0, 
    overlaying = FALSE, 
    position = 0, 
    range = c(-11.6447875867, 216.490499768), 
    rangemode = "normal", 
    showexponent = "all", 
    showgrid = TRUE, 
    showline = FALSE, 
    showticklabels = TRUE, 
    tick0 = 0, 
    tickangle = "auto", 
    tickcolor = "#444", 
    tickfont = list(
      color = "", 
      family = "", 
      size = 0
    ), 
    ticklen = 5, 
    ticks = "", 
    tickwidth = 1, 
    title = "x position [m]", 
    titlefont = list(
      color = "", 
      family = "", 
      size = 0
    ), 
    type = "linear", 
    zeroline = TRUE, 
    zerolinecolor = "#444", 
    zerolinewidth = 1
  )
)
p <- plot_ly()
p <- add_trace(p, x=trace1$x, y=trace1$y, mode=trace1$mode, name=trace1$name, type=trace1$type, uid=trace1$uid)
p <- add_trace(p, x=trace2$x, y=trace2$y, line=trace2$line, name=trace2$name, opacity=trace2$opacity, type=trace2$type, uid=trace2$uid, xaxis=trace2$xaxis, yaxis=trace2$yaxis)
p <- layout(p, annotations=layout$annotations, autosize=layout$autosize, bargap=layout$bargap, bargroupgap=layout$bargroupgap, barmode=layout$barmode, boxgap=layout$boxgap, boxgroupgap=layout$boxgroupgap, boxmode=layout$boxmode, dragmode=layout$dragmode, font=layout$font, height=layout$height, hidesources=layout$hidesources, hovermode=layout$hovermode, legend=layout$legend, margin=layout$margin, paper_bgcolor=layout$paper_bgcolor, plot_bgcolor=layout$plot_bgcolor, separators=layout$separators, showlegend=layout$showlegend, smith=layout$smith, title=layout$title, titlefont=layout$titlefont, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis)
