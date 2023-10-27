## Chemotaxis-Dashboard

Welcome to the Chemotaxis-Dashboard repository on GitHub!

Migration of neutrophils to a nidus of infection is a critical component of the host innate immune response; defects in neutrophil chemotaxis can have a severe impact on immune function. Timelapse videos of neutrophil chemotaxis in response to a chemoattractant can be a valuable tool for diagnosing rare defects, but manual collection of quantitative data from these videos can be tedious and time-consuming. We have developed an AI-based workflow to automatically segment and track the paths of individual migrating cells and provide some measures of cell shape.

The chemotaxis dashboard facilitates exploration of summary statistics for each cell (cell velocity over time and direction of movement) and for each sample (average velocity over time, average direction of movement, and the proportion of cells successfully migrating across the area of observation). Cells from different samples can be clustered, compared and contrasted based on the distribution of these summary statistics (e.g. cells obtained from healthy donors vs patients or cells responding to different chemoattractants).

The rest of this README contains lower-level notes and information on the repository itself. For additional information, checkout [this vignette](https://abcsfrederick.github.io/Chemotaxis-Dashboard/poster.html), or you can try out the shiny app with some simulated data on [shinyapps.io](https://mckalliprn.shinyapps.io/demo_app/?_ga=2.247871367.1296701277.1659379204-1106696516.1659379204).

### Setup

To setup and install your own copy of the Chemotaxis Dashboard:

* Install the Chemotaxis-Dashboard package with:

```

```

* Run `Chemotaxis-Dashboard::initialize()` - This only needs to be run once and will set up the database and other parts that the shiny app will look for.
