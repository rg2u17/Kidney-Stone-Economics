## 3. Visualise Model ####
### 3.1 Visualise Model ####
grViz(
  "
digraph MarkovModel {
  graph [layout = dot, rankdir = LR, bgcolor = 'white']

  # Nodes for model states with colors
  { rank = same; StoneFree; less_4mm; more_4mm }
   StoneFree -> less_4mm -> more_4mm [style=invis]
  { rank = same; HighRisk; LowRisk }
  LowRisk -> HighRisk [style=invis]
  { rank = same; NoRecurrence; Recurrence }
  NoRecurrence -> Recurrence [style=invis]
  
  { rank = same; FollowUp; Death }
  FollowUp -> Death [style=invis]
  
  StoneFree [shape = oval, label = 'Stone Free',
             style = filled, fillcolor = 'lightblue', color = 'darkblue']

  less_4mm [shape = oval, label = '<4mm Fragments',
             style = filled, fillcolor = 'lightblue', color = 'darkblue']

  more_4mm [shape = oval, label = '>/=4mm Fragments',
             style = filled, fillcolor = 'lightblue', color = 'darkblue']

  LowRisk [shape = rectangle, label = 'Low Risk',
           style = filled, fillcolor = 'chartreuse3', color = 'darkgreen']

  HighRisk [shape = rectangle, label = 'High Risk',
            style = filled, fillcolor = 'orange', color = 'darkorange']

  NoRecurrence [shape = rectangle, label = 'No Stone Events',
                style = filled, fillcolor = 'chartreuse3', color = 'darkgreen']

  Recurrence [shape = diamond, label = 'Stone Event',
              style = filled, fillcolor = 'salmon', color = 'red']

  FollowUp [shape = oval, label = 'No Further Stone Events\\nover Follow-up',
            style = filled, fillcolor = 'lightcyan', color = 'teal']

  Death [shape = hexagon, label = 'Death', rank = same,
         style = filled, fillcolor = 'black', color = 'black', fontcolor = 'white']

  ESWL [shape = diamond, label = 'ESWL', rank = same,
                style = filled, fillcolor = 'gold', color = 'orange']

  PCNL [shape = diamond, label = 'PCNL', rank = same,
                style = filled, fillcolor = 'gold', color = 'orange']

  URS [shape = diamond, label = 'URS', rank = same,
                style = filled, fillcolor = 'gold', color = 'orange']

  Observation [shape = diamond, label = 'Observation', rank = same,
                style = filled, fillcolor = 'gold', color = 'orange']

  # Transitions and probabilities with colored edges
  StoneFree -> LowRisk [color = 'green', fontcolor = 'darkgreen']
  StoneFree -> HighRisk [color = 'orange', fontcolor = 'darkorange']

  less_4mm -> LowRisk [color = 'green', fontcolor = 'darkgreen']
  less_4mm -> HighRisk [color = 'orange', fontcolor = 'darkorange']
  more_4mm -> LowRisk [color = 'green', fontcolor = 'darkgreen']
  more_4mm -> HighRisk [color = 'orange', fontcolor = 'darkorange']

  LowRisk -> Recurrence [color = 'red', fontcolor = 'red']
  HighRisk -> Recurrence [color = 'red', fontcolor = 'red']
  LowRisk -> NoRecurrence [color = 'darkgreen', fontcolor = 'darkgreen']
  HighRisk -> NoRecurrence [color = 'darkgreen', fontcolor = 'darkgreen']

  NoRecurrence -> FollowUp [label = 'Continue Follow-Up', color = 'teal', fontcolor = 'teal']
  Recurrence -> ESWL [label = 'ESWL (39%)', color = 'gold', fontcolor = 'orange']
  Recurrence -> PCNL [label = 'PCNL (16%)', color = 'gold', fontcolor = 'orange']
  Recurrence -> URS [label = 'URS (25%)', color = 'gold', fontcolor = 'orange']
  Recurrence -> Observation [label = 'Observation (20%)', color = 'gold', fontcolor = 'orange']

  Observation -> StoneFree [label = 'Stone Free (10%)', color = 'gold', fontcolor = 'orange']
  Observation -> less_4mm [label = 'Not Stone Free (80%)', color = 'blue', fontcolor = 'blue']
  Observation -> more_4mm [label = 'Not Stone Free (20%)', color = 'blue', fontcolor = 'blue']

  ESWL -> StoneFree [label = 'Stone Free (50%)', color = 'gold', fontcolor = 'orange']
  ESWL -> less_4mm [label = 'Not Stone Free (40%)', color = 'blue', fontcolor = 'blue']
  ESWL -> more_4mm [label = 'Not Stone Free (10%)', color = 'blue', fontcolor = 'blue']

  PCNL -> StoneFree [label = 'Stone Free (74%)', color = 'gold', fontcolor = 'orange']
  PCNL -> less_4mm [label = 'Not Stone Free (18%)', color = 'blue', fontcolor = 'blue']
  PCNL -> more_4mm [label = 'Not Stone Free (8%)', color = 'blue', fontcolor = 'blue']

  URS -> StoneFree [label = 'Stone Free (60%)', color = 'gold', fontcolor = 'orange']
  URS -> less_4mm [label = 'Not Stone Free (30%)', color = 'blue', fontcolor = 'blue']
  URS -> more_4mm [label = 'Not Stone Free (10%)', color = 'blue', fontcolor = 'blue']

  # Death state transitions with consistent styling
  HighRisk -> Death [label = 'Mortality Rate', color = 'black', fontcolor = 'black', style = 'dashed']
  LowRisk -> Death [label = 'Mortality Rate', color = 'black', fontcolor = 'black', style = 'dashed']
  Recurrence -> Death [label = 'Mortality Rate', color = 'black', fontcolor = 'black', style = 'dashed']
  NoRecurrence -> Death [label = 'Mortality Rate', color = 'black', fontcolor = 'black', style = 'dashed']
  ESWL -> Death [label = 'Mortality Rate', color = 'black', fontcolor = 'black', style = 'dashed']
  URS -> Death [label = 'Mortality Rate', color = 'black', fontcolor = 'black', style = 'dashed']
  PCNL -> Death [label = 'Mortality Rate', color = 'black', fontcolor = 'black', style = 'dashed']
  Observation -> Death [label = 'Mortality Rate', color = 'black', fontcolor = 'black', style = 'dashed']
  Death -> Death [color = 'black', fontcolor = 'black', style = 'dashed']
}
")
