plot_bias<-function(string,n,xname,ylim = NULL, mult = 0.03, string_adapt = NULL, main_paper = FALSE, silent_x = FALSE, silent_y = FALSE, y_breaks = NULL, y_labs = NULL, silent_legend = TRUE){
  require(dplyr)
  require(ggplot2)
  ADAPT = FALSE
  ll = readRDS(string)[[2]]
  if(!is.null(string_adapt)){
    ll_adapt = readRDS(string_adapt)[[2]]
    ADAPT = TRUE
  }

  #need to re-structure
  for(i in 1:length(ll)){
    temp = ll[[i]]
    counter = 1
    ll[[i]][[counter]] = (temp[[4]])
    counter = counter + 1
    ll[[i]][[counter]] = (temp[[3]])
    if(main_paper){
      counter = counter + 1
      ll[[i]][[counter]] = (temp[[1]])
    } else{
      counter = counter + 1
      ll[[i]][[counter]] = (temp[[2]])
      counter = counter + 1
      ll[[i]][[counter]] = (temp[[1]])
    }
    if(!is.null(string_adapt)){
      counter = counter + 1
      ll[[i]][[counter]] = (ll_adapt[[i]][[1]])
    }
  }

  ll_unwind = do.call(base::c,ll)
  x_pos_major = (1:length(n))/length(n)
  x_pos = NULL
  for(i in 1:length(x_pos_major)){
    temp = x_pos_major[i]
    if( (!main_paper) & ADAPT ){
      temp = c(temp, temp + mult, temp + 2*mult, temp + 3*mult, temp + 4*mult)
    } else{
      temp = c(temp, temp + mult, temp + 2*mult,  temp + 3*mult)
    }
    x_pos = c(x_pos, temp)
  }

  list_len = sapply(ll_unwind, length)
  df_x = NULL
  for(i in 1:length(x_pos)){
    df_x = c(df_x, rep(x_pos[i],list_len[i]))
  }

  df <- data.frame(
    value = do.call(base::c, ll_unwind),
    x = df_x
  )

  group_labs = NULL
  legend_labs = c('Mixed-effects','IPW estimator')
  if(main_paper){
    legend_labs = c(legend_labs, 'Balancing estimator')
  } else{
    legend_labs = c(legend_labs, 'Projection estimator', 'Balancing estimator')
  }
  if(ADAPT){
    legend_labs = c(legend_labs, 'Adaptive Balancing')
  }

  for(i in 1:length(ll)){
    group_labs = c(group_labs, rep(legend_labs[1], length(ll[[i]][[1]])))
    group_labs = c(group_labs, rep(legend_labs[2], length(ll[[i]][[2]])))
    group_labs = c(group_labs, rep(legend_labs[3], length(ll[[i]][[3]])))
    if(ADAPT & (!main_paper)){
      group_labs = c(group_labs, rep(legend_labs[4], length(ll[[i]][[4]])))
      group_labs = c(group_labs, rep(legend_labs[5], length(ll[[i]][[5]])))
    } else{
      group_labs = c(group_labs, rep(legend_labs[4], length(ll[[i]][[4]])))
    }
  }

  df$group_label = group_labs

  scale_man = scale_fill_manual(values = c(
    'IPW estimator' = colors[1],
    'Projection estimator' = colors[2],
    'Balancing estimator' = colors[3],
    'Adaptive Balancing' = colors[4],
    'Mixed-effects' = colors[5]
  ))

  if(main_paper){
    df = df %>% filter(group_label != "Projection estimator")
    scale_man = scale_fill_manual(values = c(
      'IPW estimator' = colors[1],
      'Balancing estimator' = colors[3],
      'Adaptive Balancing' = colors[4],
      'Mixed-effects' = colors[5]
    ))
  }

  if(!ADAPT){
    df = df %>% filter(group_label != "Adaptive Balancing")
    scale_man = scale_fill_manual(values = c(
      'IPW estimator' = colors[1],
      'Projection estimator' = colors[2],
      'Balancing estimator' = colors[3],
      'Mixed-effects' = colors[5]
    ))
  }

  p1 = ggplot(df, aes(x = x, y = value, group = x, fill = group_label)) +
    geom_boxplot(width = 0.15, color = "black", linewidth = 0.2, outlier.size = 0.4) +
    scale_x_continuous(breaks = x_pos_major, labels = n) +
    labs(fill = "", y = 'Bias', x = xname) +
    scale_man +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = 'none',
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 10)
    )

  if(!is.null(ylim) | !is.null(y_breaks) | !is.null(y_labs)){
    scale_args = list(limits = ylim)

    if(!is.null(y_breaks)){
      scale_args$breaks = y_breaks
    }

    if(!is.null(y_labs)){
      scale_args$labels = y_labs
    }

    p1 = p1 + do.call(scale_y_continuous, scale_args)
  }

  if(silent_x){
    p1 = p1 + labs( x = NULL)
    p1 = p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if(silent_y){
    p1 = p1 + labs( y = NULL)
    p1 = p1 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  if(!silent_legend){
    p1 = p1 + theme(legend.position = 'bottom',legend.title = element_blank(), legend.text = element_text(face = "bold") )
  } else{
    p1 = p1 + theme(legend.position = 'none')
  }

  return(p1)
}

plot_length<-function(string,n,xname,
                      ylim = NULL,
                      mult = 0.03,
                      string_adapt = NULL,
                      main_paper = FALSE,
                      silent_x = FALSE,
                      silent_y = FALSE,
                      y_breaks = NULL,
                      y_labs = NULL,
                      log_scale = TRUE,
                      y_on_log = FALSE,
                      y_tick_points = NULL){
  require(dplyr)
  require(ggplot2)

  if(y_on_log & !log_scale){
    stop("y_on_log = TRUE is only allowed when log_scale = TRUE.", call. = FALSE)
  }

  if(!is.null(y_tick_points)){
    if(any(y_tick_points <= 0)){
      stop("All entries of y_tick_points must be positive, because they are CI lengths on the natural scale.", call. = FALSE)
    }

    # y_tick_points is always supplied on the natural CI-length scale.
    # If log lengths are plotted, put ticks at log(y_tick_points).
    if(log_scale){
      y_breaks = log(y_tick_points)
    } else{
      y_breaks = y_tick_points
    }

    # Labels are always shown on the natural CI-length scale.
    y_labs = y_tick_points
  }

  ADAPT = FALSE
  ll = readRDS(string)[[2]]

  if(!is.null(string_adapt)){
    ll_adapt = readRDS(string_adapt)[[2]]
    ADAPT = TRUE
  }

  transform_length = function(x){
    if(log_scale){
      return(log(x))
    } else{
      return(x)
    }
  }

  if(log_scale & y_on_log){
    y_axis_label = "CI Length"
  } else if(log_scale){
    y_axis_label = "log(CI length)"
  } else{
    y_axis_label = "CI Length"
  }

  # need to re-structure
  for(i in 1:length(ll)){
    temp = ll[[i]]
    counter = 1

    ll[[i]][[counter]] = transform_length(temp[[4]])
    counter = counter + 1

    ll[[i]][[counter]] = transform_length(temp[[3]])

    if(main_paper){
      counter = counter + 1
      ll[[i]][[counter]] = transform_length(temp[[1]])
    } else{
      counter = counter + 1
      ll[[i]][[counter]] = transform_length(temp[[2]])
      counter = counter + 1
      ll[[i]][[counter]] = transform_length(temp[[1]])
    }

    if(!is.null(string_adapt)){
      counter = counter + 1
      ll[[i]][[counter]] = transform_length(ll_adapt[[i]][[1]])
    }
  }

  ll_unwind = do.call(c,ll)
  x_pos_major = (1:length(n))/length(n)

  x_pos = NULL
  for(i in 1:length(x_pos_major)){
    temp = x_pos_major[i]

    if((!main_paper) & ADAPT){
      temp = c(temp, temp + mult, temp + 2*mult, temp + 3*mult, temp + 4*mult)
    } else{
      temp = c(temp, temp + mult, temp + 2*mult, temp + 3*mult)
    }

    x_pos = c(x_pos, temp)
  }

  list_len = sapply(ll_unwind, length)

  df_x = NULL
  for(i in 1:length(x_pos)){
    df_x = c(df_x, rep(x_pos[i], list_len[i]))
  }

  df <- data.frame(
    value = do.call(c, ll_unwind),
    x = df_x
  )

  group_labs = NULL

  legend_labs = c('Mixed-effects','IPW estimator')

  if(main_paper){
    legend_labs = c(legend_labs, 'Balancing estimator')
  } else{
    legend_labs = c(legend_labs, 'Projection estimator', 'Balancing estimator')
  }

  if(ADAPT){
    legend_labs = c(legend_labs, 'Adaptive Balancing')
  }

  for(i in 1:length(ll)){
    group_labs = c(group_labs, rep(legend_labs[1], length(ll[[i]][[1]])))
    group_labs = c(group_labs, rep(legend_labs[2], length(ll[[i]][[2]])))
    group_labs = c(group_labs, rep(legend_labs[3], length(ll[[i]][[3]])))

    if(ADAPT & (!main_paper)){
      group_labs = c(group_labs, rep(legend_labs[4], length(ll[[i]][[4]])))
      group_labs = c(group_labs, rep(legend_labs[5], length(ll[[i]][[5]])))
    } else{
      group_labs = c(group_labs, rep(legend_labs[4], length(ll[[i]][[4]])))
    }
  }

  df$group_label = group_labs

  scale_man = scale_fill_manual(values = c(
    'IPW estimator' = colors[1],
    'Projection estimator' = colors[2],
    'Balancing estimator' = colors[3],
    'Adaptive Balancing' = colors[4],
    'Mixed-effects' = colors[5]
  ))

  if(main_paper){
    df = df %>% filter(group_label != "Projection estimator")

    scale_man = scale_fill_manual(values = c(
      'IPW estimator' = colors[1],
      'Balancing estimator' = colors[3],
      'Adaptive Balancing' = colors[4],
      'Mixed-effects' = colors[5]
    ))
  }

  if(!ADAPT){
    df = df %>% filter(group_label != "Adaptive Balancing")

    scale_man = scale_fill_manual(values = c(
      'IPW estimator' = colors[1],
      'Projection estimator' = colors[2],
      'Balancing estimator' = colors[3],
      'Mixed-effects' = colors[5]
    ))
  }

  p1 = ggplot(df, aes(x = x, y = value, group = x, fill = group_label)) +
    geom_boxplot(width = 0.15, color = "black", linewidth = 0.2, outlier.size = 0.4) +
    scale_x_continuous(breaks = x_pos_major, labels = n) +
    labs(fill = "", y = y_axis_label, x = xname) +
    scale_man +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = 'none',
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 10)
    )

  # Add y-scale carefully.
  # Do not pass labels = NULL, because that can suppress tick labels.
  if(!is.null(ylim) | !is.null(y_breaks) | !is.null(y_labs) | y_on_log){
    scale_args = list(limits = ylim)

    if(!is.null(y_breaks)){
      scale_args$breaks = y_breaks
    }

    if(!is.null(y_labs)){
      scale_args$labels = y_labs
    } else if(y_on_log){
      scale_args$labels = function(x) round(exp(x), digits = 2)
    }

    p1 = p1 + do.call(scale_y_continuous, scale_args)
  }

  if(silent_x){
    p1 = p1 + labs(x = NULL)
    p1 = p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if(silent_y){
    p1 = p1 + labs(y = NULL)
    p1 = p1 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  return(p1)
}



plot_coverage<-function(string,n,xname,ylim = c(-10,10),se_mult = 2, mult = 0.03, string_adapt = NULL, coverage = NULL, main_paper = FALSE, silent_x = FALSE, silent_y = FALSE){
  require(dplyr)
  require(ggplot2)
  ADAPT = FALSE
  ll = readRDS(string)[[1]]
  if(!is.null(string_adapt)){
    ll_adapt = readRDS(string_adapt)[[1]]
    ADAPT = TRUE
  }
  x_pos_major = (1:length(n))/length(n)

  if(main_paper){
    df = data.frame(y = c(ll[[13]], ll[[9]], ll[[1]], ll_adapt[[1]]), se = c(ll[[14]], ll[[10]],ll[[2]], ll_adapt[[2]]), x = c(x_pos_major, x_pos_major + mult, x_pos_major+2*mult, x_pos_major+3*mult), group_label = c(rep('Mixed-effects',length(n)) ,rep('IPW estimator',length(n)), rep('Balancing estimator',length(n)), rep('Adaptive Balancing',length(n))) )
  } else{
    if(ADAPT){
      df = data.frame(y = c(ll[[13]] ,ll[[9]], ll[[5]] ,ll[[1]], ll_adapt[[1]]), se = c(ll[[14]], ll[[10]], ll[[6]], ll[[2]], ll_adapt[[2]]), x = c(x_pos_major, x_pos_major + mult, x_pos_major+2*mult, x_pos_major+3*mult, x_pos_major+4*mult), group_label = c(rep('Mixed-effects',length(n)), rep('IPW estimator',length(n)),rep('Projection estimator',length(n)) ,rep('Balancing estimator',length(n)), rep('Adaptive Balancing',length(n))))
    } else{
      df = data.frame(y = c(ll[[13]] , ll[[9]], ll[[5]] ,ll[[1]]), se = c(ll[[14]], ll[[10]], ll[[6]], ll[[2]]), x = c(x_pos_major, x_pos_major + mult, x_pos_major+2*mult, x_pos_major+3*mult), group_label = c(rep('Mixed-effects',length(n)) ,rep('IPW estimator',length(n)),rep('Projection estimator',length(n)) ,rep('Balancing estimator',length(n))) )
    }
  }

  scale_man = scale_color_manual(values = c(
    'IPW estimator' = colors[1],
    'Projection estimator' = colors[2],
    'Balancing estimator' = colors[3],
    'Adaptive Balancing' = colors[4],
    'Mixed-effects' = colors[5]
  ))

  shape_man = scale_shape_manual(values = c(
    'IPW estimator' = shapes[1],
    'Projection estimator' = shapes[2],
    'Balancing estimator' = shapes[3],
    'Adaptive Balancing' = shapes[4],
    'Mixed-effects' = shapes[5]
  ))

  if(main_paper){
    scale_man = scale_color_manual(values = c(
      'IPW estimator' = colors[1],
      'Balancing estimator' = colors[3],
      'Adaptive Balancing' = colors[4],
      'Mixed-effects' = colors[5]
    ))

    shape_man = scale_shape_manual(values = c(
      'IPW estimator' = shapes[1],
      'Balancing estimator' = shapes[3],
      'Adaptive Balancing' = shapes[4],
      'Mixed-effects' = shapes[5]
    ))
  }

  if(!ADAPT){
    scale_man = scale_color_manual(values = c(
      'IPW estimator' = colors[1],
      'Projection estimator' = colors[2],
      'Balancing estimator' = colors[3],
      'Mixed-effects' = colors[5]
    ))

    shape_man = scale_shape_manual(values = c(
      'IPW estimator' = shapes[1],
      'Projection estimator' = shapes[2],
      'Balancing estimator' = shapes[3],
      'Mixed-effects' = shapes[5]
    ))
  }

  p1 = ggplot(df, aes(x = x, y = y, group = x, color = group_label, shape = group_label, fill = group_label)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = y - se_mult*se, ymax = y + se_mult*se), width = 0.01) +
    scale_x_continuous(breaks = x_pos_major, labels = n) +
    scale_man +
    shape_man +
    labs(fill = "", y = 'CI Coverage', x = xname) +
    geom_hline(yintercept = 0.95, linetype = "dotted", color = "black", linewidth = 0.8) +
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = 'none',
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 10)
    )

  if(!is.null(ylim)){
    p1 = p1 + scale_y_continuous(limits = ylim)
  }

  if(silent_x){
    p1 = p1 + labs( x = NULL)
    p1 = p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if(silent_y){
    p1 = p1 + labs( y = NULL)
    p1 = p1 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  return(p1)
}

plot_sd<-function(string,n,xname,
                  ylim = NULL,
                  mult = 0.03,
                  string_adapt = NULL,
                  main_paper = FALSE,
                  silent_x = FALSE,
                  silent_y = FALSE,
                  silent_legend = TRUE,
                  legend_nrow = 1){
  require(dplyr)
  require(ggplot2)
  ADAPT = FALSE
  ll = readRDS(string)[[1]]
  if(!is.null(string_adapt)){
    ll_adapt = readRDS(string_adapt)[[1]]
    ADAPT = TRUE
  }
  x_pos_major = (1:length(n))/length(n)

  get_entry = function(x, name_options, index){
    x_names = names(x)
    if(!is.null(x_names)){
      matched_name = intersect(name_options, x_names)
      if(length(matched_name) > 0){
        return(x[[matched_name[1]]])
      }
    }
    x[[index]]
  }

  check_len = function(x, label){
    if(length(x) != length(n)){
      stop(paste0(label, " has length ", length(x), ", but n has length ", length(n)), call. = FALSE)
    }
    x
  }

  sd_ME = check_len(get_entry(ll, c('variance.ME', 'sd.ME'), 14), 'Mixed-effects estimator std. dev.')
  sd_ipw = check_len(get_entry(ll, c('variance.ipw', 'sd.ipw'), 17), 'IPW estimator std. dev.')
  sd_bal_ipw = check_len(get_entry(ll, c('variance.bal_ipw', 'sd.bal_ipw'), 16), 'Projection estimator std. dev.')
  sd_bal = check_len(get_entry(ll, c('variance.bal', 'sd.bal'), 15), 'Balancing estimator std. dev.')
  if(ADAPT){
    sd_adapt = check_len(get_entry(ll_adapt, c('variance.adapt', 'sd.adapt', 'variance.bal', 'sd.bal'), 15), 'Adaptive Balancing std. dev.')
  }

  if(main_paper){
    df = data.frame(
      y = c(sd_ME, sd_ipw, sd_bal, sd_adapt),
      x = c(x_pos_major, x_pos_major + mult, x_pos_major+2*mult, x_pos_major+3*mult),
      group_label = c(
        rep('Mixed-effects',length(n)),
        rep('IPW estimator',length(n)),
        rep('Balancing estimator',length(n)),
        rep('Adaptive Balancing',length(n))
      )
    )
  } else{
    if(ADAPT){
      df = data.frame(
        y = c(sd_ME, sd_ipw, sd_bal_ipw, sd_bal, sd_adapt),
        x = c(x_pos_major, x_pos_major + mult, x_pos_major+2*mult, x_pos_major+3*mult, x_pos_major+4*mult),
        group_label = c(
          rep('Mixed-effects',length(n)),
          rep('IPW estimator',length(n)),
          rep('Projection estimator',length(n)),
          rep('Balancing estimator',length(n)),
          rep('Adaptive Balancing',length(n))
        )
      )
    } else{
      df = data.frame(
        y = c(sd_ME, sd_ipw, sd_bal_ipw, sd_bal),
        x = c(x_pos_major, x_pos_major + mult, x_pos_major+2*mult, x_pos_major+3*mult),
        group_label = c(
          rep('Mixed-effects',length(n)),
          rep('IPW estimator',length(n)),
          rep('Projection estimator',length(n)),
          rep('Balancing estimator',length(n))
        )
      )
    }
  }

  scale_man = scale_color_manual(values = c(
    'IPW estimator' = colors[1],
    'Projection estimator' = colors[2],
    'Balancing estimator' = colors[3],
    'Adaptive Balancing' = colors[4],
    'Mixed-effects' = colors[5]
  ))

  shape_man = scale_shape_manual(values = c(
    'IPW estimator' = shapes[1],
    'Projection estimator' = shapes[2],
    'Balancing estimator' = shapes[3],
    'Adaptive Balancing' = shapes[4],
    'Mixed-effects' = shapes[5]
  ))

  if(main_paper){
    scale_man = scale_color_manual(values = c(
      'IPW estimator' = colors[1],
      'Balancing estimator' = colors[3],
      'Adaptive Balancing' = colors[4],
      'Mixed-effects' = colors[5]
    ))

    shape_man = scale_shape_manual(values = c(
      'IPW estimator' = shapes[1],
      'Balancing estimator' = shapes[3],
      'Adaptive Balancing' = shapes[4],
      'Mixed-effects' = shapes[5]
    ))
  }

  if(!ADAPT){
    scale_man = scale_color_manual(values = c(
      'IPW estimator' = colors[1],
      'Projection estimator' = colors[2],
      'Balancing estimator' = colors[3],
      'Mixed-effects' = colors[5]
    ))

    shape_man = scale_shape_manual(values = c(
      'IPW estimator' = shapes[1],
      'Projection estimator' = shapes[2],
      'Balancing estimator' = shapes[3],
      'Mixed-effects' = shapes[5]
    ))
  }

  p1 = ggplot(df, aes(x = x, y = y, group = x, color = group_label, shape = group_label, fill = group_label)) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = x_pos_major, labels = n) +
    scale_man +
    shape_man +
    labs(y = 'Estimator Std. Dev.', x = xname) +
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 10)
    )

  if(!is.null(ylim)){
    p1 = p1 + scale_y_continuous(limits = ylim)
  }

  if(silent_x){
    p1 = p1 + labs( x = NULL)
    p1 = p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if(silent_y){
    p1 = p1 + labs( y = NULL)
    p1 = p1 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  if(!silent_legend){
    p1 = p1 +
      guides(
        color = guide_legend(nrow = legend_nrow, byrow = TRUE),
        shape = guide_legend(nrow = legend_nrow, byrow = TRUE)
      ) +
      theme(
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 9),
        legend.box = "vertical",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.25, "cm")
      )
  } else{
    p1 = p1 + theme(legend.position = 'none')
  }

  return(p1)
}


colors = c("#0072B2", "#E69F00", "chartreuse4", "#D55E00", '#6A5ACD')

# Shape mapping:
# IPW estimator        -> 16 solid circle
# Projection estimator -> 15 solid square
# Balancing estimator  -> 17 solid triangle
# Adaptive Balancing   -> 8 star
# Mixed-effects        -> 18 solid diamond, purple via colors[5]
shapes = c(16,15,17,8,18)

n_vary_ci = c(100,300,500,700)
n_vary_est =  c(50,75,100,125)
lopsided_vary = c(0.20, 1.65, 3.10, 4.55, 6.00)
snr_vary = c(0.2, 0.5, 1, 2, 5)

# main paper
p1 = plot_sd('~/output_files/graph_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(0,0.32),  mult = 0.01, string_adapt = '~/output_files/graph_ci_adapt_fixed.list', main_paper = TRUE, silent_x = TRUE)
p2 = plot_sd('~/output_files/graph_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(0,0.32),  mult = 0.01, string_adapt = '~/output_files/graph_ci_snr_adapt_fixed.list', main_paper = TRUE, silent_x = TRUE, silent_y = TRUE)
p3 = plot_sd('~/output_files/graph_ci_lopsided_fixed.list', n = lopsided_vary, xname =  "Counterfactual deviation",  ylim = c(0,0.32),  mult = 0.01, string_adapt = '~/output_files/graph_ci_lopsided_adapt_fixed.list', main_paper = TRUE, silent_x = TRUE, silent_y = TRUE)

p4 = plot_coverage('~/output_files/graph_ci_fixed.list', n = n_vary_ci, xname ="Number of clusters",se_mult = 2,  ylim = c(0,1),  mult = 0.01, string_adapt = '~/output_files/graph_ci_adapt_fixed.list', coverage = 0.95, main_paper = TRUE, silent_x = TRUE)
p5 = plot_coverage('~/output_files/graph_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(0,1), se_mult = 2, mult = 0.01, string_adapt = '~/output_files/graph_ci_snr_adapt_fixed.list', coverage = 0.95, main_paper = TRUE, silent_x = TRUE, silent_y = TRUE)
p6 = plot_coverage('~/output_files/graph_ci_lopsided_fixed.list', n = lopsided_vary, xname =  "Counterfactual deviation",  ylim = c(0,1), se_mult = 2, mult = 0.01, string_adapt = '~/output_files/graph_ci_lopsided_adapt_fixed.list', coverage = 0.95, main_paper = TRUE, silent_x = TRUE, silent_y = TRUE)

p7 = plot_length('~/output_files/graph_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(-4,1),  mult = 0.01, string_adapt = '~/output_files/graph_ci_adapt_fixed.list', main_paper = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))
p8 = plot_length('~/output_files/graph_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(-4,1),  mult = 0.01, string_adapt = '~/output_files/graph_ci_snr_adapt_fixed.list',  main_paper = TRUE, silent_y = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))
p9 = plot_length('~/output_files/graph_ci_lopsided_fixed.list', n = lopsided_vary, xname = "Counterfactual deviation",  ylim = c(-4,1),  mult = 0.01, string_adapt = '~/output_files/graph_ci_lopsided_adapt_fixed.list', main_paper = TRUE, silent_y = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))

p0 = plot_sd('~/output_files/graph_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(0,0.06),  mult = 0.01, string_adapt = '~/output_files/graph_ci_adapt_fixed.list', main_paper = TRUE, silent_x = TRUE, silent_legend = FALSE)

library(patchwork)
library(cowplot)
plots <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9)
plot_grid <- wrap_plots(plots, ncol = 3)
legend = get_legend(p0)
final_plot <- plot_grid(
  plot_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)
ggsave('~/Figures/main_paper.pdf',plot = final_plot,width=6.5,height=6)


# stratified CI total
p1 = plot_sd('~/output_files/stratified_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(0,0.25),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_x = TRUE)
p2 = plot_sd('~/output_files/stratified_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(0,0.25),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)
p3 = plot_sd('~/output_files/stratified_ci_lopsided_fixed.list', n = lopsided_vary, xname =  "Counterfactual deviation",  ylim = c(0,0.25),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)

p4 = plot_coverage('~/output_files/stratified_ci_fixed.list', n = n_vary_ci, xname ="Number of clusters",se_mult = 2,  ylim = c(0,1),  mult = 0.01, string_adapt = NULL, coverage = 0.95, main_paper = FALSE, silent_x = TRUE)
p5 = plot_coverage('~/output_files/stratified_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(0,1), se_mult = 2, mult = 0.01, string_adapt = NULL, coverage = 0.95, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)
p6 = plot_coverage('~/output_files/stratified_ci_lopsided_fixed.list', n = lopsided_vary, xname =  "Counterfactual deviation",  ylim = c(0,1), se_mult = 2, mult = 0.01, string_adapt = NULL, coverage = 0.95, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)

p7 = plot_length('~/output_files/stratified_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(-4,1),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))
p8 = plot_length('~/output_files/stratified_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(-4,1),  mult = 0.01, string_adapt = NULL,  main_paper = FALSE, silent_y = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))
p9 = plot_length('~/output_files/stratified_ci_lopsided_fixed.list', n = lopsided_vary, xname = "Counterfactual deviation",  ylim = c(-4,1),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_y = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))

p0 = plot_sd('~/output_files/stratified_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(0,0.06),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_x = TRUE, silent_legend = FALSE)

library(patchwork)
library(cowplot)
plots <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9)
plot_grid <- wrap_plots(plots, ncol = 3)
legend = get_legend(p0)
final_plot <- plot_grid(
  plot_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)
ggsave('~/Figures/ci_stratified.pdf',plot = final_plot, width=6.5,height=6)


# graph CI total
p1 = plot_sd('~/output_files/graph_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(0,0.32),  mult = 0.01, string_adapt = '~/output_files/graph_ci_adapt_fixed.list', main_paper = FALSE, silent_x = TRUE)
p2 = plot_sd('~/output_files/graph_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(0,0.32),  mult = 0.01, string_adapt = '~/output_files/graph_ci_snr_adapt_fixed.list', main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)
p3 = plot_sd('~/output_files/graph_ci_lopsided_fixed.list', n = lopsided_vary, xname =  "Counterfactual deviation",  ylim = c(0,0.32),  mult = 0.01, string_adapt = '~/output_files/graph_ci_lopsided_adapt_fixed.list', main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)

p4 = plot_coverage('~/output_files/graph_ci_fixed.list', n = n_vary_ci, xname ="Number of clusters",se_mult = 2,  ylim = c(0,1),  mult = 0.01, string_adapt = '~/output_files/graph_ci_adapt_fixed.list', coverage = 0.95, main_paper = FALSE, silent_x = TRUE)
p5 = plot_coverage('~/output_files/graph_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(0,1), se_mult = 2, mult = 0.01, string_adapt = '~/output_files/graph_ci_snr_adapt_fixed.list', coverage = 0.95, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)
p6 = plot_coverage('~/output_files/graph_ci_lopsided_fixed.list', n = lopsided_vary, xname =  "Counterfactual deviation",  ylim = c(0,1), se_mult = 2, mult = 0.01, string_adapt = '~/output_files/graph_ci_lopsided_adapt_fixed.list', coverage = 0.95, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)

p7 = plot_length('~/output_files/graph_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(-4,1),  mult = 0.01, string_adapt = '~/output_files/graph_ci_adapt_fixed.list', main_paper = FALSE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))
p8 = plot_length('~/output_files/graph_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(-4,1),  mult = 0.01, string_adapt = '~/output_files/graph_ci_snr_adapt_fixed.list',  main_paper = FALSE, silent_y = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))
p9 = plot_length('~/output_files/graph_ci_lopsided_fixed.list', n = lopsided_vary, xname = "Counterfactual deviation",  ylim = c(-4,1),  mult = 0.01, string_adapt = '~/output_files/graph_ci_lopsided_adapt_fixed.list', main_paper = FALSE, silent_y = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))

p0 = plot_sd(
  '~/output_files/graph_ci_fixed.list',
  n = n_vary_ci,
  xname = "Number of clusters",
  ylim = c(0,0.06),
  mult = 0.01,
  string_adapt = '~/output_files/graph_ci_adapt_fixed.list',
  main_paper = FALSE,
  silent_x = TRUE,
  silent_legend = FALSE,
  legend_nrow = 2
)

library(patchwork)
library(cowplot)
plots <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9)
plot_grid <- wrap_plots(plots, ncol = 3)
legend = get_legend(p0)
final_plot <- plot_grid(
  plot_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)
ggsave('~/Figures/ci_graph.pdf',plot = final_plot,width=6.5,height=6)


# additive CI total
p1 = plot_sd('~/output_files/additive_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(0,0.2),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_x = TRUE)
p2 = plot_sd('~/output_files/additive_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(0,0.2),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)
p3 = plot_sd('~/output_files/additive_ci_lopsided_fixed.list', n = lopsided_vary, xname =  "Counterfactual deviation",  ylim = c(0,0.2),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)

p4 = plot_coverage('~/output_files/additive_ci_fixed.list', n = n_vary_ci, xname ="Number of clusters",se_mult = 2,  ylim = c(0,1),  mult = 0.01, string_adapt = NULL, coverage = 0.95, main_paper = FALSE, silent_x = TRUE)
p5 = plot_coverage('~/output_files/additive_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(0,1), se_mult = 2, mult = 0.01, string_adapt = NULL, coverage = 0.95, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)
p6 = plot_coverage('~/output_files/additive_ci_lopsided_fixed.list', n = lopsided_vary, xname =  "Counterfactual deviation",  ylim = c(0,1), se_mult = 2, mult = 0.01, string_adapt = NULL, coverage = 0.95, main_paper = FALSE, silent_x = TRUE, silent_y = TRUE)

p7 = plot_length('~/output_files/additive_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(-4,1),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))
p8 = plot_length('~/output_files/additive_ci_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = c(-4,1),  mult = 0.01, string_adapt = NULL,  main_paper = FALSE, silent_y = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))
p9 = plot_length('~/output_files/additive_ci_lopsided_fixed.list', n = lopsided_vary, xname = "Counterfactual deviation",  ylim = c(-4,1),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_y = TRUE, log_scale = TRUE, y_on_log = TRUE, y_tick_points = c(0.01, 0.05, 0.25, 0.5, 1, 2.5))

p0 = plot_sd('~/output_files/additive_ci_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = c(0,0.06),  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_x = TRUE, silent_legend = FALSE)

library(patchwork)
library(cowplot)
plots <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9)
plot_grid <- wrap_plots(plots, ncol = 3)
legend = get_legend(p0)
final_plot <- plot_grid(
  plot_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)
ggsave('~/Figures/ci_additive.pdf',plot = final_plot,width=6.5,height=6)
# stratified bias
p1 = plot_bias('~/output_files/stratified_est_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE)
p2 = plot_bias('~/output_files/stratified_est_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = NULL,  mult = 0.01, string_adapt = NULL,  main_paper = FALSE)
p3 = plot_bias('~/output_files/stratified_est_lopsided_fixed.list', n = lopsided_vary, xname = "Counterfactual deviation",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE)

p0 = plot_bias('~/output_files/stratified_est_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_legend = FALSE)
library(patchwork)
library(cowplot)
plots <- list(p1, p2, p3)
plot_grid <- wrap_plots(plots, ncol = 3)
legend = get_legend(p0)
final_plot <- plot_grid(
  plot_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)
ggsave('~/Figures/bias_stratified.pdf',plot =final_plot,width=6.5,height=6)


# graph bias
p1 = plot_bias('~/output_files/graph_est_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE)
p2 = plot_bias('~/output_files/graph_est_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = NULL,  mult = 0.01, string_adapt = NULL,  main_paper = FALSE)
p3 = plot_bias('~/output_files/graph_est_lopsided_fixed.list', n = lopsided_vary, xname = "Counterfactual deviation",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE)

p0 = plot_bias('~/output_files/stratified_est_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_legend = FALSE)
library(patchwork)
library(cowplot)
plots <- list(p1, p2, p3)
plot_grid <- wrap_plots(plots, ncol = 3)
legend = get_legend(p0)
final_plot <- plot_grid(
  plot_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)
ggsave('~/Figures/bias_graph.pdf',plot = final_plot,width=6.5,height=6)


# additive bias
p1 = plot_bias('~/output_files/additive_est_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE)
p2 = plot_bias('~/output_files/additive_est_snr_fixed.list', n = snr_vary, xname = 'Signal-to-noise ratio',  ylim = NULL,  mult = 0.01, string_adapt = NULL,  main_paper = FALSE)
p3 = plot_bias('~/output_files/additive_est_lopsided_fixed.list', n = lopsided_vary, xname = "Counterfactual deviation",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE)

p0 = plot_bias('~/output_files/stratified_est_fixed.list', n = n_vary_ci, xname = "Number of clusters",  ylim = NULL,  mult = 0.01, string_adapt = NULL, main_paper = FALSE, silent_legend = FALSE)
library(patchwork)
library(cowplot)
plots <- list(p1, p2, p3)
plot_grid <- wrap_plots(plots, ncol = 3)
legend = get_legend(p0)
final_plot <- plot_grid(
  plot_grid,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)
ggsave('~/Figures/bias_additive.pdf',plot = final_plot,width=6.5,height=6)

