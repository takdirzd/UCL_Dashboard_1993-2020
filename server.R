function(input, output, session) { 
  
  # ---------------------------------- PLOT 1  SCORED--------------------------------- 
  
  output$plot1 <- renderEcharts4r({
    
  # SCORED
    
  top_goals <- stats %>% 
    
    group_by(team) %>% 
    summarise(total_goals = sum(goals_scored)) %>% 
    ungroup() %>% 
    arrange(-total_goals) %>%  
    slice_head(n=10) 
    
  
  
  # x <- as.numeric(rownames(stats))
  # 
  # top_goals <- top_goals[1:which(input$bins==x),]
  
  top_goals %>% 
    arrange(total_goals) %>% 
    e_chart(x = team) %>% 
    e_bar(total_goals, legend = F, name = "Scored") %>%
    e_grid(left = "20%") %>% 
    e_color("#588dd5") %>% 
    e_theme_custom("walden.json") %>% 
    e_tooltip() %>%
    e_title("Top 10 Team with Most Goal Scored All Time", "UEFA Champions League Season 1992-1993 to 2019-2020") %>% 
    e_flip_coords() %>%
    e_y_axis(splitLine = list(show = F)) %>%
    e_x_axis(show = FALSE) 
  })
  

  
  # ---------------------------------- PLOT 1 ADD ------------------------------
  
  output$plot1add <- renderEcharts4r({
    
    # SCORED
    
    top_goals1 <- stats %>% 
      filter(year == input$input_yearadd) %>% 
      group_by(team) %>% 
      summarise(total_goals = sum(goals_scored)) %>% 
      ungroup() %>% 
      arrange(-total_goals) 
    
    
    
    x <- as.numeric(rownames(stats))
    
    top_goals1 <- top_goals1[1:which(input$bins==x),]
    
    top_goals1 %>% 
      arrange(total_goals) %>% 
      e_chart(x = team) %>% 
      e_bar(total_goals, legend = F, name = "Scored") %>%
      e_grid(left = "20%") %>% 
      e_color("#588dd5") %>% 
      e_theme_custom("walden.json") %>% 
      e_tooltip() %>%
      e_title(glue("Top 10 Team with Most Goal Scored season {input$input_yearadd}"), "UEFA Champions League Season 1992-1993 to 2019-2020") %>% 
      e_flip_coords() %>%
      e_y_axis(splitLine = list(show = F)) %>%
      e_x_axis(show = FALSE) 
  })
  
  # ---------------------------------- PLOT 2  CONCEDED--------------------------------- 
  
  output$plot2 <- renderEcharts4r({
   
  # CONCEDED 
  
  top_con <- stats %>% 
    
    group_by(team) %>% 
    summarise(total_conceded = sum(goals_conceded)) %>% 
    ungroup() %>% 
    arrange(-total_conceded) %>%  
    slice_head(n=10)
 
  
  # x <- as.numeric(rownames(top_con))
  # 
  # top_con <- top_con[1:which(input$bins1==x),]
  
  # VIZ
  
  top_con %>% 
    arrange(total_conceded) %>% 
    e_chart(x = team) %>% 
    e_bar(total_conceded, legend = F, name = "Conceded") %>% 
    e_grid(left = "20%") %>% 
    e_color("#c05050") %>% 
    e_theme_custom("walden.json") %>% 
    e_tooltip() %>%
    e_title("Top 10 Team with Most Goal Conceded All Time", "UEFA Champions League Season 1992-1993 to 2019-2020") %>% 
    e_flip_coords() %>%
    e_y_axis(splitLine = list(show = F)) %>%
    e_x_axis(show = FALSE)
  
  
  })
  
  # ---------------------------------- PLOT 2 ADD ------------------------------
  
  output$plot2add <- renderEcharts4r({
    
    # SCORED
    
    top_con1 <- stats %>% 
      filter(year == input$input_yearadd) %>% 
      group_by(team) %>% 
      summarise(total_goals = sum(goals_scored)) %>% 
      ungroup() %>% 
      arrange(-total_goals) 
    
    
    
    x2 <- as.numeric(rownames(stats))
    
    top_con1 <- top_con1[1:which(input$bins==x2),]
    
    top_con1 %>% 
      arrange(total_goals) %>% 
      e_chart(x = team) %>% 
      e_bar(total_goals, legend = F, name = "Conceded") %>%
      e_grid(left = "20%") %>% 
      e_color("#c05050") %>% 
      e_theme_custom("walden.json") %>% 
      e_tooltip() %>%
      e_title(glue("Top 10 Team with Most Goal Conceded season {input$input_yearadd}"), "UEFA Champions League Season 1992-1993 to 2019-2020") %>% 
      e_flip_coords() %>%
      e_y_axis(splitLine = list(show = F)) %>%
      e_x_axis(show = FALSE) 
  })
  
  
  
  # ---------------------------------- PLOT 3  MATCH RESULT--------------------------------- 
  
  output$plot3 <- renderEcharts4r({
  
  # MATCH RESULT
    
    mu <- stats %>% 
      filter(team == input$input_team) %>% 
      group_by(team) %>% 
      summarise(wins = sum(wins),
                draws = sum(draws),
                losts = sum(losts)) %>% 
      ungroup()
    mu$team <-  droplevels(mu$team)
    
    
    mu_result <- pivot_longer(data = mu,
                              cols = c("wins", "draws","losts"))
    mu_result <- mu_result %>% rename(Total = value)  
  
    # VIZ
    
    mu_result %>% 
      e_charts(name) %>% 
      e_bar(Total) %>% 
      e_tooltip(
        trigger = "axis",
        axisPointer = list(
          type = "shadow"
        )
      ) %>% 
      e_title(
        text = glue('{input$input_team} Match Results All Time'),left = "center",
        subtext = 'UEFA Champions League from 1993-2020'
      ) %>% 
      e_legend(show = F) %>% 
      e_theme_custom("walden.json") %>% 
      e_y_axis(
        splitArea = list(show = FALSE),
        splitLine = list(show = FALSE)
      )
  })  
  
  # ---------------------------------- PLOT 4  GOAL PERFORM--------------------------------- 
  
  output$plot4 <- renderEcharts4r({
    
    # GOAL PERFORM
    
    mug <- stats %>% 
      filter(team == input$input_team) %>% 
      group_by(year, team) %>% 
      summarise(goals_scored = sum(goals_scored),
                goals_conceded = sum(goals_conceded)) %>% 
      ungroup()
    
    mug$team <-  droplevels(mug$team)
    
    mug <- mug %>% rename(Scored = goals_scored)
    mug <- mug %>% rename(Conceded = goals_conceded)
    
    mug_result <- pivot_longer(data = mug,
                               cols = c("Scored", "Conceded"))
    mug_result <- mug_result %>% rename(Total = value) 
    
    mug_result <- mug_result %>% 
      group_by(name) %>% 
      arrange(year) %>% 
      mutate(
        name = as.factor(name)
      )
    
    # VIZ
    
    max <- list(
      name = "Max",
      type = "max"
    )
    min <- list(
      name = "Min",
      type = "min"
    )
    
    mug_result %>%  
      e_charts(year) %>% 
      #   e_datazoom(
      #   type = "slider", 
      #   toolbox = FALSE,
      #   bottom = -5
      # ) %>% 
      e_line(Total) %>%
      e_legend(show=F) %>% 
      e_tooltip() %>% 
      e_mark_point(data = max) %>% 
      e_mark_point(data = min) %>% 
      e_title(
        text = glue('{input$input_team} Goal Performance All Time'), left = "center",
        subtext = 'UEFA Champions League from 1993-2020'
      ) %>% 
      e_theme_custom("walden.json") %>% 
      e_x_axis(year, axisPointer = list(show = TRUE)) %>% 
      e_y_axis(
        splitArea = list(show = FALSE),
        splitLine = list(show = FALSE)
      ) %>% 
      e_color(c("#c05050","#588dd5"))
  })
  
  
  # ---------------------------------- PLOT 5  HEAD TO HEAD--------------------------------- 
  
  output$plot5 <- renderEcharts4r({
    
  # HEAD TO HEAD
  
  # TEAM 1
  ce <- stats %>% 
    filter(team == input$input_team2 & year == input$input_year2) %>% 
    group_by(team) %>% 
    summarise(wins = sum(wins),
              draws = sum(draws),
              losts = sum(losts)) %>% 
    ungroup()
  ce$team <-  droplevels(ce$team)
  
  
  ce_result <- pivot_longer(data = ce,
                            cols = c("wins", "draws","losts"))
  ce_result <- ce_result %>% rename(Total = value)  
  
  
  # TEAM 2
  rm <- stats %>% 
    filter(team == input$input_team21 & year == input$input_year2) %>% 
    group_by(team) %>% 
    summarise(wins = sum(wins),
              draws = sum(draws),
              losts = sum(losts)) %>% 
    ungroup()
  rm$team <-  droplevels(rm$team)
  
  
  rm_result <- pivot_longer(data = rm,
                            cols = c("wins", "draws","losts"))
  
  rm_result <- rm_result %>% rename(Total = value) 
  
  # JOIN
  
  hth <- full_join(ce_result,rm_result)
  
  # VIZ
  
  hth %>% 
    group_by(team) %>% 
    e_charts(name) %>% 
    e_bar(Total, stack = "grp2") %>% 
    e_tooltip(
      trigger = "axis",
      axisPointer = list(
        type = "shadow"
      )
    ) %>% 
    e_theme_custom("walden.json") %>% 
    e_title(
      text = glue('{input$input_team2} vs {input$input_team21} '),left = "center",
      subtext = glue('Match Result UEFA Champions League Season {input$input_year2}'),
      left = "center"
    ) %>% 
    e_legend(show=F)
  
  
  })
  
  # ---------------------------------- PLOT 6  GOAL PERFORM TEAM 1--------------------------------- 
  
  output$plot6 <- renderEcharts4r({
    

  
    # TEAM 1
    ceg <- stats %>% 
      filter(team == input$input_team2 ) %>% 
      #filter(team == "Chelsea" & (year >= 2015 & year <= 2016)) %>% 
      group_by(year, team) %>% 
      summarise(goals_scored = sum(goals_scored),
                goals_conceded = sum(goals_conceded)) %>% 
      ungroup()
    ceg$team <-  droplevels(ceg$team)
    
    ceg <- ceg %>% rename(Scored = goals_scored)
    ceg <- ceg %>% rename(Conceded = goals_conceded)
    
    ceg_result <- pivot_longer(data = ceg,
                               cols = c("Scored", "Conceded"))
    ceg_result <- ceg_result %>% rename(Total = value) 
    
    ceg_result <- ceg_result %>% 
      group_by(name) %>% 
      arrange(year) 
    
    # VIZ
    
    max <- list(
      name = "Max",
      type = "max"
    )
    min <- list(
      name = "Min",
      type = "min"
    )
    
    ceg_result %>%
      e_charts(year) %>% 
      #   e_datazoom(
      #   type = "slider", 
      #   toolbox = FALSE,
      #   bottom = -5
      # ) %>% 
      
      e_line(Total, legend = T)   %>% 
      e_mark_point(data = max) %>% 
      e_mark_point(data = min) %>% 
      e_tooltip() %>% 
      e_title(
        text = glue('{input$input_team2} Goal Performance'), left = "center"
      ) %>% 
      e_theme_custom("walden.json") %>% 
      e_x_axis(year, axisPointer = list(show = TRUE)) %>% 
      e_y_axis(
        splitArea = list(show = FALSE),
        splitLine = list(show = FALSE)
      ) %>% 
      e_legend(show=F) %>% 
      e_color(c("#c05050","#588dd5"))
    
  })
  
  # ---------------------------------- PLOT 7  GOAL PERFORM TEAM 2--------------------------------- 
  
  output$plot7 <- renderEcharts4r({
    
    # TEAM 2
    rmg <- stats %>% 
      filter(team == input$input_team21 ) %>% 
      group_by(year, team) %>% 
      summarise(goals_scored = sum(goals_scored),
                goals_conceded = sum(goals_conceded)) %>% 
      ungroup()
    rmg$team <-  droplevels(rmg$team)
    
    rmg <- rmg %>% rename(Scored = goals_scored)
    rmg <- rmg %>% rename(Conceded = goals_conceded)
    
    rmg_result <- pivot_longer(data = rmg,
                               cols = c("Scored", "Conceded"))
    rmg_result <- rmg_result %>% rename(Total = value) 
    
    # VIZ
    
    max <- list(
      name = "Max",
      type = "max"
    )
    min <- list(
      name = "Min",
      type = "min"
    )
    
    rmg_result <- rmg_result %>% 
      group_by(name) %>% 
      arrange(year) 
    
    rmg_result %>%
      e_charts(year) %>% 
      #   e_datazoom(
      #   type = "slider", 
      #   toolbox = FALSE,
      #   bottom = -5
      # ) %>% 
      
      e_line(Total, legend = T)   %>% 
      e_mark_point(data = max) %>% 
      e_mark_point(data = min) %>% 
      e_tooltip() %>% 
      e_title(
        text = glue('{input$input_team21} Goal Performance'),left = "center"
      ) %>% 
      e_theme_custom("walden.json") %>% 
      e_x_axis(year, axisPointer = list(show = TRUE)) %>% 
      e_y_axis(
        splitArea = list(show = FALSE),
        splitLine = list(show = FALSE)
        
      ) %>% 
      e_legend(show=F)%>% 
      e_color(c("#c05050","#588dd5"))
    
  })
  
  # ---------------------------------- PLOT 8  GROUP POINT--------------------------------- -------------------------------------------------
  
  output$plot8 <- renderEcharts4r({
    
  # GROUP POINT
    
    points <- stats %>% 
    select(c(year, team, group_point, champions)) %>% 
    filter(year == input$input_year3) %>% 
    group_by(year) %>% 
    arrange(-group_point) 
  
    xg <- as.numeric(rownames(stats))
    
    points <- points[1:which(input$binsg==xg),]
  
  # VIZ
  
  points %>%
    arrange(group_point) %>%
    e_chart(x = team) %>%
    e_bar(group_point, legend = F, name = "Points") %>%
    e_theme_custom("walden.json") %>%
    e_grid(left = "20%") %>% 
    e_tooltip(trigger = "item") %>%
    e_title("Top 10 Team points obtained in the group stage in a season", glue("UEFA Champions League Season {input$input_year3}")) %>%
    e_flip_coords() %>%
    e_y_axis(splitLine = list(show = F)) %>%
    e_x_axis(show = FALSE)
    
    
  })
  
  # ---------------------------------- PLOT 8 ADD  GROUP POINT--------------------------------- 
  
  output$plot8add <- renderEcharts4r({

  # GROUP POINT
  
  top_champ <- stats %>% 
    
    group_by(team) %>% 
    summarise(champions = sum(champions)) %>% 
    ungroup() %>% 
    arrange(-champions) %>%  
    slice_head(n=10) 
  

  
  top_champ %>%
    arrange(champions) %>%
    e_chart(x = team) %>%
    e_bar(champions, legend = F, name = "Total Champions") %>%
    e_theme_custom("walden.json") %>%
    e_grid(left = "20%") %>%
    e_tooltip(trigger = "item") %>%
    e_title("Top 10 Teams with the most champions", glue("UEFA Champions League Season 1992-1993 to 2019-2020")) %>%
    e_flip_coords() %>%
    e_y_axis(splitLine = list(show = F)) %>%
    e_x_axis(show = FALSE)
  })
 
  
  # ------------------------------- VALUE BOX 0 PAGE 1--------------------------------
  
  output$vbox <- renderValueBox({
    
    mug0 <- stats %>%
      filter(year == input$input_yearadd) %>%
      group_by(year, team) %>% 
      arrange(-goals_scored)
  
    hcmug0 <- hchart(mug0, "column", hcaes(team, match_played), name = "Match Played")  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    valueBoxSpark(
      value = glue("Team Played : {nrow(mug0)}"),
      title = toupper("Total Team "),
      sparkobj = hcmug0,
      subtitle = glue("UEFA Champions League Season {input$input_yearadd}"),
      icon = icon("people-group"),
      width = 4,
      color = "blue",
      href = NULL
    )
    
    
  })
  
  
  # ------------------------------- VALUE BOX 1  PAGE 1--------------------------------
  
  output$vbox2 <- renderValueBox({
    
    mug1 <- stats %>%
      filter(team == "Real Madrid") %>%
      group_by(year, team) %>%
      summarise(goals_scored = sum(goals_scored),
                goals_conceded = sum(goals_conceded)) %>%
      ungroup()

    mug1$team <-  droplevels(mug1$team)

    
    hcmug1 <- hchart(mug1, "line", hcaes(year, goals_scored), name = "Goal Scored")  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    valueBoxSpark(
      value = "Real Madrid",
      title = toupper("Top Goal Scored All Time"),
      sparkobj = hcmug1,
      subtitle = "UEFA Champions League Season 1992-1993 to 2019-2020",
      icon = icon("arrow-up"),
      width = 4,
      color = "blue",
      href = NULL
    )
    
    
  })
  
  # ------------------------------- VALUE BOX 2  PAGE 1--------------------------------
  
  output$vbox3 <- renderValueBox({
    
    mug2 <- stats %>%
      filter(team == "Real Madrid") %>%
      group_by(year, team) %>%
      summarise(goals_scored = sum(goals_scored),
                goals_conceded = sum(goals_conceded)) %>%
      ungroup()
    
    mug2$team <-  droplevels(mug2$team)

    
    
    hcmug2 <- hchart(mug2, "line", hcaes(year, goals_conceded), name = "Goal Conceded")  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    valueBoxSpark(
      value = "Real Madrid",
      title = toupper("Top Goal Conceded All Time"),
      sparkobj = hcmug2,
      subtitle = "UEFA Champions League Season 1992-1993 to 2019-2020",
      icon = icon("arrow-down"),
      width = 4,
      color = "red",
      href = NULL
    )
    
    
  })
  
  # ------------------------------- VALUE BOX 1 RESULT PAGE 2 --------------------------------
  
  output$vboxres <- renderValueBox({
    
    tr <- stats %>% 
      filter(team == input$input_team) %>% 
      group_by(year, champions) %>%
      summarise(champions = sum(champions)) %>% 
      ungroup()
    
    trh <- stats %>% 
      filter(team == input$input_team) %>% 
      summarise(champions = sum(champions)) %>% 
      ungroup()
    
    trv <- hchart(tr, "line", hcaes(year, champions), name = "Champion")  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    
    valueBoxSpark(
      value = glue("Total Champion : {trh}"),
      title = toupper(glue("{input$input_team}")),
      sparkobj = trv,
      subtitle = "UEFA Champions League Season 1992-1993 to 2019-2020",
      icon = icon("trophy"),
      color = "green",
      
    )
    
    
  })
  
  # ------------------------------- VALUE BOX 2 RESULT PAGE 2 --------------------------------
  
  output$vboxres2 <- renderValueBox({
    
    trmp <- stats %>% 
      filter(team == input$input_team) %>% 
      group_by(year, match_played) %>%
      summarise(match_played = sum(match_played)) %>% 
      ungroup()
    
    trhmp <- stats %>% 
      filter(team == input$input_team) %>% 
      summarise(match_played = sum(match_played)) %>% 
      ungroup()
    
    trvmp <- hchart(trmp, "line", hcaes(year, match_played), name = "Match Played")  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    
    valueBoxSpark(
      value = glue("Total Match Played : {trhmp}"),
      title = toupper(glue("{input$input_team}")),
      sparkobj = trvmp,
      subtitle = "UEFA Champions League Season 1992-1993 to 2019-2020",
      icon = icon("play"),
      color = "blue",
      
    )
    
    
  })
  
  
  # ------------------------------- VALUE BOX 1 HEAD TO HEAD PAGE 3--------------------------------
  
  output$vboxhth <- renderValueBox({
    
    rmc <- stats %>% 
      filter(team == input$input_team2) %>% 
      group_by(year, champions) %>%
      summarise(champions = sum(champions)) %>% 
      ungroup()
    
    rmch <- stats %>% 
      filter(team == input$input_team2) %>% 
      summarise(champions = sum(champions)) %>% 
      ungroup()
    
    rmh <- hchart(rmc, "line", hcaes(year, champions), name = "Champion")  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    rmh
    
    valueBoxSpark(
      value = glue("{input$input_team2}"),
      title = toupper(glue("Total Champion : {rmch}")),
      sparkobj = rmh,
      subtitle = "UEFA Champions League Season 1992-1993 to 2019-2020",
      
      width = 4,
      color = "blue",
      
    )
    
    
  })
  
  # ------------------------------- VALUE BOX 2 HEAD TO HEAD PAGE 3--------------------------------
  
  output$vboxhth2 <- renderValueBox({
    
    # TEAM 1
    cevb <- stats %>% 
      filter(team == input$input_team2) %>% 
      group_by(team) %>% 
      summarise(wins = sum(wins),
                draws = sum(draws),
                losts = sum(losts)) %>% 
      ungroup()
    cevb$team <-  droplevels(cevb$team)
    
    
    cevb_result <- pivot_longer(data = cevb,
                                cols = c("wins", "draws","losts"))
    cevb_result <- cevb_result %>% rename(Total = value)   
    cevb_result
    
    
    
    # TEAM 2
    rmvb <- stats %>% 
      filter(team == input$input_team21) %>% 
      group_by(team) %>% 
      summarise(wins = sum(wins),
                draws = sum(draws),
                losts = sum(losts)) %>% 
      ungroup()
    rmvb$team <-  droplevels(rmvb$team)
    
    
    rmvb_result <- pivot_longer(data = rmvb,
                                cols = c("wins", "draws","losts"))
    
    rmvb_result <- rmvb_result %>% rename(Total = value) 

    
    # JOIN
    
    hthvb <- full_join(cevb_result,rmvb_result)
    
    hthvbv <- hchart(hthvb, "column", hcaes(x = 'name', y = 'Total', group = 'team'))  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_xAxis(visible =F) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    
    valueBoxSpark(
      value = glue("Result Match All Time"),
      title = toupper(glue("{input$input_team2} vs {input$input_team21}")),
      sparkobj = hthvbv,
      subtitle = "UEFA Champions League Season 1992-1993 to 2019-2020",
      
      width = 4,
      color = "blue",
      
    )
    
    
  })
  
  # ------------------------------- VALUE BOX 3 HEAD TO HEAD PAGE 3--------------------------------
  
  output$vboxhth3 <- renderValueBox({
    
    bc <- stats %>% 
      filter(team == input$input_team21) %>% 
      group_by(year, champions) %>%
      summarise(champions = sum(champions)) %>% 
      ungroup()
    
    bch <- stats %>% 
      filter(team == input$input_team21) %>% 
      summarise(champions = sum(champions)) %>% 
      ungroup()
    
    bh <- hchart(bc, "line", hcaes(year, champions), name = "Champion")  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
   
    
    valueBoxSpark(
      value = glue("{input$input_team21}"),
      title = toupper(glue("Total Champion : {bch}")),
      sparkobj = bh,
      subtitle = "UEFA Champions League Season 1992-1993 to 2019-2020",
      
      width = 4,
      color = "blue",
      
    )
    
    
  })
  
  # ------------------------------- VALUE BOX 1 CHAMP PAGE 4--------------------------------
  
  output$vboxchamp <- renderValueBox({
    
    champ <- stats %>%
      filter(year == input$input_year3 & champions == 1) %>%
      group_by(year, team) %>%
      summarise(goals_scored = sum(goals_scored),
                goals_conceded = sum(goals_conceded)) %>%
      ungroup()
    
    champ$team <-  droplevels(champ$team)
    
    
    valueBoxSpark(
      value = champ$team,
      title = toupper("UEFA Champions League WINNER"),
      
      subtitle =  glue("UEFA Champions League Season {input$input_year3}"),
      icon = icon("trophy"),
      width = 6,
      color = "green",
      href = NULL
    )
    
    
  })
  
  # ------------------------------- VALUE BOX 2 CHAMP PAGE 4--------------------------------
  
  output$vbox2champ <- renderValueBox({
    
    champ <- stats %>%
      filter(year == input$input_year3 & champions == 1) %>%
      group_by(year, team) %>%
      summarise(goals_scored = sum(goals_scored),
                goals_conceded = sum(goals_conceded)) %>%
      ungroup()
    
    champ$team <-  droplevels(champ$team)
    
    
    valueBoxSpark(
      value = champ$goals_scored,
      title = toupper("GOALS SCORED"),
      sparkobj = ,
      subtitle =  glue("UEFA Champions League Season {input$input_year3}"),
      icon = icon("futbol"),
      width = 4,
      color = "green",
      href = NULL
    )
    
    
  })
  
  # ------------------------------- VALUE BOX 3 CHAMP PAGE 4--------------------------------
  
  output$vbox3champ <- renderValueBox({
    
    top_champ <- stats %>% 
      
      group_by(team) %>% 
      summarise(champions = sum(champions)) %>% 
      ungroup() %>% 
      arrange(-champions) %>%  
      slice_head(n=10) 
    
    
    top_champs <- top_champ %>% 
      filter(team == "Real Madrid")
    
    valueBoxSpark(
      value = top_champs[,1],
      title = toupper("MOST CHAMPIONS"),
      
      subtitle = "UEFA Champions League Season 1992-1993 to 2019-2020",
      icon = icon("crown"),
      width = 4,
      color = "blue",
      href = NULL
    )
    
    
  })
  
  
} # FUNCTION 