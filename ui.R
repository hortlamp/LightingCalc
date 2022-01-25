#NO DB Lighting Calc Version: ui.r

tagList( #needed for shinyjs
  useShinyalert(),
  useShinyjs(),  # Include shinyjs
  introjsUI(),
  withMathJax(),
  uiOutput('uibody') #UI from the renderUI function that creates the UI on the server side. renderUI  lets you generate calls to UI functions and make the results appear in a predetermined place in the UI.
)