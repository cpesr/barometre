load("results.RData")

labvarfun <- function(x,pad,wrap=50) {
  x <- str_pad(x, pad, side = "left")
  x <- str_wrap(x,wrap)
  x <- str_replace(x," ; ","\n")
#  x <- str_replace(x,"ATEN, ","ATEN,\n")
  x[is.na(x)] <- "Non renseigné"
  return(x)
}

plot_caracteristique <- function(variable, title="", palette="Set2", pad=0, wrap=65, width=0.9) {
  results %>%
    rename(var = !!sym(variable)) %>%
    summarise(nb = n(), .by = var) %>%
    mutate(part = nb / sum(nb)) %>%
    #mutate(var = fct_explicit_na(var,"Sans réponse")) %>%
    filter(!is.na(var)) %>%
    ggplot(aes(y=var,x=nb,fill=var)) + 
    geom_col(color="black",size=0.1,width = width) +
    scale_y_discrete(name=variable, limits=rev, labels= ~ labvarfun(.x,pad,wrap) ) +
    #scale_x_continuous(labels=scales::percent, name = "Part des réponses") +
    scale_x_continuous(name = "Nombre de répondants") +
    scale_fill_brewer(palette=palette, na.value="grey", direction = -1) +
    ggtitle(title) +
    theme(legend.position = "none", 
          axis.title.y = element_blank(),
          plot.title.position = "plot", plot.title = element_text(hjust = 1))
}

#plot_caracteristique('sexe')
#plot_caracteristique('categorie',"Catégorie")

plot_etab <- function() {
  results %>%
    mutate(typeetab.other. = ifelse(is.na(typeetab.other.),"Non","Oui")) %>%
    pivot_longer(starts_with("typeetab"), names_to = "Type", values_to = "Affectation") %>%
    summarise(Nombre = n(), .by = c(Type,Affectation)) %>%
    mutate(part = Nombre/sum(Nombre), .by = Type) %>%
    mutate(Type = factor(Type,levels=etab.factor$levels,labels=etab.factor$labels)) %>%
    arrange(Type,Affectation) %>%
    filter(Affectation == "Oui") %>%
    ggplot(aes(y=reorder(Type,-Nombre),x=Nombre,fill=Type)) + 
      geom_col(color="black",size=0.1) +
      scale_y_discrete(name="Type d'établissement", limits=rev) +
      # scale_x_continuous(labels=scales::percent, name = "Part des réponses") +
      scale_x_continuous(name = "Nombre de répondants") +
      ggtitle("Quel est votre établissement ?") +
      theme(legend.position = "none", 
            axis.title.y = element_blank(),
            plot.title.position = "plot", plot.title = element_text(hjust = 1))
}

#plot_etab()


plot_bloc <- function(bloc,bloc.factor, filldir=0, qwrap=50) {
  df <- results %>%
    select(id,starts_with(bloc)) %>%
    pivot_longer(-id, values_to = "Réponse", names_to = "Question") %>%
    filter(!is.na(Réponse)) %>%
    filter( !startsWith(as.character(Réponse), "Ne connais pas")) %>%
    mutate(Réponse = droplevels(Réponse)) %>%
    summarise(nb = n(), .by=c(Question,Réponse)) %>% 
    #mutate(Question = factor(Question)) %>%
    mutate(
      Question = factor(Question,
                        levels = paste0(bloc,".",bloc.factor$levels),
                        labels = bloc.factor$labels) )  %>%
    arrange(Question,Réponse) %>% 
    mutate(
      part = nb / sum(nb), .by = Question,
      v1 = cumsum(part),
      v2 = lag(v1,default = 0),
      mid = sum(ifelse(as.numeric(Réponse) == ceiling(length(levels(Réponse))/2),(v1+v2) / 2,0))) %>%
    mutate(
      v1 = v1 - mid,
      v2 = v2- mid,
    ) 
  
  df %>%
    mutate(RéponseNum = as.numeric(gsub(" .*","",Réponse))) %>%
    mutate(Question = fct_rev(Question)) %>%
    ggplot(aes(xmin=v2, xmax=v1, 
               y=Question, ymin=as.numeric(Question)-0.4, ymax=as.numeric(Question)+0.4,
               fill=RéponseNum)) +
    geom_rect(alpha=0.8,color="black", size=0.1) +
    geom_vline(xintercept = 0, size=0.1, color="black") +
    scale_x_continuous(limits=c(-1,1), name = "Part des répondants", 
                       labels = ~ scales::percent(abs(.x))) +
    scale_y_discrete(labels = ~ str_wrap(.x,width=qwrap), name=NULL) +
    scale_fill_distiller(palette='RdYlBu', direction=filldir, breaks=seq(0,10),
                         guide = guide_coloursteps(), name=NULL)
}


#plot_bloc("conditions",conditions.factor)
#plot_bloc("evolution",conditions.factor)
#plot_bloc("optimisme",conditions.factor)
#plot_bloc("confiance",confiance.factor)
#plot_bloc("PCinquietude",pc.factor,filldir=-1)
#plot_bloc("PCimpact",pc.factor,filldir=-1)
#plot_bloc("PCeffort",pc.factor)
#plot_bloc("socle",socle.factor)


plot_bloc_hist <- function(bloc,bloc.factor) {
  
  results %>%
    select(id,starts_with(bloc)) %>%
    pivot_longer(-id, values_to = "Réponse", names_to = "Question") %>%
    filter(!is.na(Réponse)) %>%
    filter( !startsWith(as.character(Réponse), "Ne connais pas")) %>%
    mutate(Réponse = droplevels(Réponse)) %>%
    summarise(nb = n(), .by=c(Question,Réponse)) %>%
    mutate(
      Question = factor(Question,
                        levels = paste0(bloc,".",bloc.factor$levels),
                        labels = bloc.factor$labels) )  %>%
    arrange(Question,Réponse) %>% 
    mutate(part = nb / sum(nb), .by = Question) %>%
    ggplot(aes(x=Réponse,y=part)) + geom_col() + facet_grid(Question~.)
  
}
#plot_bloc_hist("conditions",conditions.factor)


plot_bloc_connait <- function(bloc,bloc.factor) {
  results %>%
    select(id,starts_with(bloc)) %>%
    pivot_longer(-id, values_to = "Réponse", names_to = "Question") %>%
    filter(!is.na(Réponse)) %>%
    mutate(Réponse = droplevels(Réponse)) %>%
    summarise(nb = n(), .by=c(Question,Réponse)) %>%
    mutate(
      Question = factor(Question,
                        levels = paste0(bloc,".",bloc.factor$levels),
                        labels = bloc.factor$labels) )  %>%
    arrange(Question,Réponse) %>%
    mutate(part = nb / sum(nb), .by = Question) %>%
    complete(Question,Réponse,fill = list(part=0)) %>%
    filter(startsWith(as.character(Réponse), "Ne connais pas")) %>%
    
    ggplot(aes(x=part, y=Question, fill=Réponse)) +
    geom_col(alpha=0.8, color="black", size=0.1) +
    scale_x_continuous(name = "Part des répondants", 
                       labels = ~ scales::percent(abs(.x))) +
    scale_y_discrete(limits=rev, labels = ~ str_wrap(.x,45)) +
    scale_fill_brewer(palette='Dark2') 
}

# plot_bloc_connait("confiance",confiance.factor)

plot_bloc_grid <- function(bloc,bloc.factor,filldir=0) {
  p1 <- plot_bloc(bloc,bloc.factor,filldir) + theme(legend.title = element_blank())
  p2 <- plot_bloc_connait(bloc,bloc.factor) + 
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(), legend.title = element_blank())
  
  cowplot::plot_grid(nrow=1, rel_widths = c(6,2), align = "h", p1, p2)
}


# plot_bloc_grid("confiance",confiance.factor)
# plot_bloc_grid("reformes",reformes.factor)



plot_bloc_percent <- function(bloc, pad=35, plot=TRUE,filldir=0) {
  df <- results %>%
    select(id,starts_with(bloc)) %>%
    pivot_longer(-id, values_to = "Réponse", names_to = "Question") %>%
    filter(!is.na(Réponse)) %>%
    filter( !startsWith(as.character(Réponse), "Ne connais pas")) %>%
    mutate(Réponse = droplevels(Réponse)) %>%
    summarise(nb.questions = n(), .by=Réponse) %>%
    mutate(part = nb.questions / sum(nb.questions)) %>%
    arrange(Réponse)
  
  if(!plot) return(
    df %>% 
      mutate(cs = scales::percent(cumsum(part))) %>%
      arrange(desc(Réponse)) %>%
      mutate(csr = scales::percent(cumsum(part)))
  )
                     
  part <- df %>%
    filter(as.numeric(Réponse) > length(levels(Réponse))/2+1) %>%
    summarise(part = scales::percent(sum(part))) %>%
    pull(part)

  p <- df %>%
    ggplot(aes(x=1,y=part,fill=Réponse,color=Réponse)) +
    geom_col(color="black",size=0.1) +
    annotate("text",x=-2,y=0,label=part,size=18,fontface="bold") +
    coord_polar(theta="y") +
    xlim(c(-2, 1.5)) +
    #scale_color_brewer(palette='RdYlBu', labels = ~ str_pad(.x, pad, side = "right")) +
    scale_fill_brewer(palette='RdYlBu', labels = ~ str_pad(.x, pad, side = "right"), direction=filldir) +
    theme_void() +
    theme(legend.position = "left")
  return(p)
}
# plot_bloc_percent("evolution")
# plot_bloc_percent("optimisme")
# plot_bloc_percent("confiance")
# plot_bloc_percent("reformes")

# plot_bloc_percent("conditions", plot=FALSE)
# plot_bloc_percent("evolution", plot=FALSE)
# plot_bloc_percent("optimisme", plot=FALSE)
# plot_bloc_percent("confiance", plot=FALSE)
# plot_bloc_percent("reformes", plot=FALSE)




nb_repondants <- function(variable,valeur=NA) {
  df <- results %>%
    summarise(nb=n(),.by=(!!sym(variable)))
  
  if(!is.na(valeur)) 
    return(df %>% filter(!!sym(variable) == valeur) %>% pull(nb))
  
  return(df)
}


plot_pop2 <- function(variable,blocs,bloc.factor,bloc.labels,palette="Set2",size=4) {
  df <- results %>%
    rename(var = !!sym(variable)) %>%
    select(id,var,starts_with(blocs)) %>%
    pivot_longer(-c(id,var), values_to = "Réponse", names_to = "Question") %>% 
    filter(!is.na(Réponse),!is.na(var)) %>%
    filter( !startsWith(as.character(Réponse), "Ne connais pas")) %>% 
    mutate(bloc = factor(
      gsub("\\..*","",Question),
      levels = blocs,
      labels = bloc.labels
      )) %>% 
    mutate(Question = factor(
      gsub("([A-Za-z]*)\\.(.*)","\\2",Question),
      levels = bloc.factor$levels,
      labels = bloc.factor$lab,
    )) %>% 
    mutate(Réponse2 = droplevels(recode_factor(Réponse,"Ne connais pas" = NA_character_))) %>%
    mutate(Score = as.numeric(Réponse2)-6) %>%
    summarise(Score = mean(Score,na.rm=TRUE), .by=c(var,bloc,Question)) %>%
    mutate(Score.diff = Score - mean(Score), .by=c(bloc,Question)) %>%
    arrange(var,bloc,Question) 
  
  # return(df)
  

  df %>%
    ggplot(aes(x=Score.diff, y=Question, shape=var, fill=var)) +
    geom_vline(xintercept = 0, color="grey") +
    geom_point(size=size, stroke=0.2) +
    facet_wrap(bloc~.) +
    scale_y_discrete(limits=rev,name="") +
    scale_fill_brewer(palette=palette, name="", direction=-1) +
    scale_x_continuous(name="Ecart au score moyen") +
    scale_shape_manual(name="", values=c(21,24,22,23,25,20)) +
    theme(legend.position = "right", strip.text.x = element_text(size = 14))
}

#plot_pop2("categorie.grp",c("PCinquietude","PCimpact","PCeffort"), pc.factor, c("Inquiétude","Impact","Effort"),"Set1")
#plot_pop2("sexe",c("PCinquietude","PCimpact","PCeffort"),pc.factor,c("Inquiétude","Impact","Effort"), palette)



plot_pops <- function(variable, palette="Set2", angle = 0) {
  
    cowplot::plot_grid(ncol=1,
      plot_pop2(variable,c("conditions","optimisme","evolution"), conditions.factor, c("Conditions","Optimisme","Evolution"), palette) ,
      cowplot::plot_grid(ncol=3, rel_widths = c(1.5,1.5,1),
        plot_pop2(variable,c("PCinquietude","PCimpact","PCeffort"), pc.factor, c("Inquiétude","Impact","Effort"), palette) + theme(legend.position = "None"),
        plot_pop2(variable,"confiance", confiance.factor,"Confiance", palette,2)+ theme(legend.position = "None"),
        plot_caracteristique(variable, palette = palette, width=0.5) + 
          scale_y_discrete(limits=identity) +
          coord_flip() + 
          ggtitle("Nombre de répondants") +
          theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = angle)) 
        )
    )
}

# plot_pops("sexe.grp")
# plot_pops("anciennete","PRGn")
# plot_pops("metier.grp","Set1")
# plot_pops("statut.grp","Accent")
# plot_pops("discipline.grp","Accent")
# plot_pops("categorie.grp","Dark2")
# plot_pops("responsabilites.grp","Oranges")


