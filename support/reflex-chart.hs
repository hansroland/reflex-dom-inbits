-- ----------------------------------------------------------------------
-- Create the diagrams to explain Events, Behaviors and Dynamics
-- ----------------------------------------------------------------------
--
-- run from main directory with     runghc support/reflex-chart.hs
--
-- -----------------------------------------------------------------------
import Graphics.Rendering.Chart hiding (x0, y0)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens

chartEvent = toRenderable layout
  where
    eventPlot = plot_points_style .~ filledCircles 10 (opaque orange)
              $ plot_points_values .~ events
              -- $ plot_points_title .~ "events"
              $ def

    layout = layoutXAxis
           $ layoutYAxis
           $ layout_plots .~ [ toPlot eventPlot]
           $ def

chartBehavior = toRenderable layout
  where
    behaviorPlot = plot_lines_values .~ behaviors
                 $ plot_lines_style .~ solidLine 2.0 (opaque red) 
                 $ def
    layout = layoutXAxis
           $ layoutYAxis
           $ layout_plots .~ [ toPlot behaviorPlot]
           $ def

chartDynamic = toRenderable layout
  where
    behaviorPlot = plot_lines_values .~ behaviors
                 $ plot_lines_style .~ solidLine 2.0 (opaque red) 
                 $ def
    eventPlot = plot_points_style .~ filledCircles 10 (opaque orange)
              $ plot_points_values .~ events
              -- $ plot_points_title .~ "events"
              $ def

    layout = layoutXAxis
           $ layoutYAxis
           $ layout_plots .~ [ toPlot eventPlot, toPlot behaviorPlot]
           $ def



layoutXAxis :: Layout Double Double -> Layout Double Double
layoutXAxis = layout_x_axis . laxis_generate .~ const AxisData {
               _axis_visibility = def,
               _axis_viewport = vmap (minx,maxx),
               _axis_tropweiv = invmap (minx,maxx),
               _axis_ticks    = [],
               _axis_grid     = xValues,
               _axis_labels   = [[(x1,"Event"), (x2, "Event"), (x3, "Event"), (x4,"Event")], [(x5, "time ->")]]
              }

layoutYAxis :: Layout x0 Double -> Layout x0 Double
layoutYAxis = layout_y_axis . laxis_generate .~ const AxisData {
               _axis_visibility = def,
               _axis_viewport = vmap (miny,maxy),
               _axis_tropweiv = invmap (miny,maxy),
               _axis_ticks    = [],
               _axis_grid     = [],
               _axis_labels   = [[(y4, "Value a")]]
              }




main :: IO ()
main = do
  renderableToFile fileoptions "images/event.png" chartEvent
  renderableToFile fileoptions "images/behavior.png" chartBehavior
  renderableToFile fileoptions "images/dynamic.png" chartDynamic
  return ()

miny,maxy :: Double
(miny,maxy) = (-0.9, 3.6)

minx,maxx :: Double
(minx,maxx) = (-0.9, 7.0)

fileoptions = FileOptions {
  _fo_size = (440, 220),
  _fo_format = PNG
  }

-- --------------------------------------------------------
-- Data
-- --------------------------------------------------------
x1 = 0
x2 = 2
x3 = 4
x4 = 5
x5 = 7

y1 = 0
y2 = 2
y3 = 1
y4 = 3

xValues = [x1, x2, x3, x4]


events :: [(Double, Double)]
events = [(x1,y1), (x2,y2), (x3,y3), (x4, y4)]

ends :: [(Double, Double)]
ends = [(x2,y1), (x3, y2), (x4, y3), (x5, y4)]

behaviors = [[(x1,y1), (x2,y1)], [(x2, y2), (x3, y2)], [(x3,y3), (x4,y3)], [(x4,y4), (x5,y4)]]
