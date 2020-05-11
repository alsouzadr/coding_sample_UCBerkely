## This show a simple plot I did using some eye tracking data while I was a postdoc at Concordia University in Canada
## The data is not available to reproduce the plots

## TIME COURSE ADULTS
## Looking at TARGET
target=c()
participant=c()
trial=c()
interval=c()
trial_number=c()
participants=levels(adult$RecordingName)
minha.lista=lapply(participants,function(x) adult[adult$RecordingName==x,]) ## creates a separate dataset for each participant
for(i in 1:length(minha.lista)){ ## Goes through a file for each participant
	part.i=minha.lista[[i]]
		for(j in sort(unique(part.i$trial.number))){ ## Goes for each trial
			trial1=part.i[part.i$trial.number==j,]
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
			trial1=trial1[trial1$is.target=="yes",] ## TARGET OBJECT
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
			trial1=trial1[order(trial1$RecordingTimestamp,decreasing=F),]
			trial1$time=(trial1$RecordingTimestamp-min(trial1$RecordingTimestamp))/1000
			trial1$breaks=cut(trial1$RecordingTimestamp,breaks=160,labels=1:160) ## breaks trial to intervals of roughly 0.1 seconds
			trial1=trial1[trial1$GazeEventType=="Fixation",]
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
				for(k in unique(trial1$breaks)){
					break1=trial1[trial1$breaks==k,]
					yes=sum(break1[break1$looking_aoi=="yes",]$GazeEventDuration)
					target=append(target,yes)
					participant=append(participant,as.vector(unique(break1$RecordingName)))
					trial=append(trial,as.vector(unique(break1$trial.type)))
					interval=append(interval,as.vector(unique(break1$breaks)))
					trial_number=append(trial_number,as.vector(unique(break1$trial.number)))
				}
		}
}
plot_target=data.frame(participant,trial,trial_number,interval,target)

## Looking at NON_TARGET
non_target=c()
participant=c()
trial=c()
interval=c()
trial_number=c()
participants=levels(adult$RecordingName)
minha.lista=lapply(participants,function(x) adult[adult$RecordingName==x,]) ## creates a separate dataset for each participant
for(i in 1:length(minha.lista)){ ## Goes through a file for each participant
	part.i=minha.lista[[i]]
		for(j in sort(unique(part.i$trial.number))){ ## Goes for each trial
			trial1=part.i[part.i$trial.number==j,]
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
			trial1=trial1[trial1$is.target=="no",] ## DISTRACTOR OBJECT
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
			trial1=trial1[order(trial1$RecordingTimestamp,decreasing=F),]
			trial1$time=(trial1$RecordingTimestamp-min(trial1$RecordingTimestamp))/1000
			trial1$breaks=cut(trial1$RecordingTimestamp,breaks=160,labels=1:160) ## breaks trial to intervals of roughly 0.1 seconds
			trial1=trial1[trial1$GazeEventType=="Fixation",]
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
				for(k in unique(trial1$breaks)){
					break1=trial1[trial1$breaks==k,]
					yes=sum(break1[break1$looking_aoi=="yes",]$GazeEventDuration)
					non_target=append(non_target,yes)
					participant=append(participant,as.vector(unique(break1$RecordingName)))
					trial=append(trial,as.vector(unique(break1$trial.type)))
					interval=append(interval,as.vector(unique(break1$breaks)))
					trial_number=append(trial_number,as.vector(unique(break1$trial.number)))
				}
		}
}
plot_distractor=data.frame(participant,trial,trial_number,interval,non_target)


plot_dataa=data.frame(plot_target,plot_distractor$non_target)
colnames(plot_dataa)=c("part","trial","trial_number","interval","target","distractor")
plot_dataa$both=plot_dataa$target+plot_dataa$distractor
plot_dataa=plot_dataa[plot_dataa$both>0,]
plot_dataa$prop=plot_dataa$target/(plot_dataa$target+plot_dataa$distractor)
plot_dataa$interval=as.numeric(as.character(plot_dataa$interval))
time_coursea=aggregate(prop~interval+trial,data=plot_dataa,mean)
time_coursea$time=time_coursea$interval*51
time_coursea=time_coursea[time_coursea$time>3000,]

ggplot(data=time_coursea,aes(x=time, y=prop,colour=trial))+geom_line(aes(shape=trial,colour=trial))+theme_bw()+ylim(0,1)+geom_point(aes(shape=trial,colour=trial),size = 2.3)+geom_vline(xintercept=c(5400), linetype="dotted")+xlab("Time in Milliseconds")+ylab("Proportion of Time Looking at the target object")


## CHILDREN 20 MONTHS
## Looking at TARGET
children20=children_merged[children_merged$age.group==20,]
children20=as.data.frame(lapply(children20,function(x) x[,drop=TRUE])) ## drop unsused levels

## Excluding some other columns
drops=c("SegmentStart","StudioTestName","ParticipantName","StudioProjectName")
children=children20[,!(names(children20)%in%drops)]

## Only the keepers (repeating)
children=children[children$keeper==1,]
children=as.data.frame(lapply(children,function(x) x[,drop=TRUE])) ## drop unsused levels

## Only valid trials
children=children[children$trial.include==1,]
children=as.data.frame(lapply(children,function(x) x[,drop=TRUE])) ## drop unsused levels

## Exclude fillers
children=children[children$trial.type!="filler",]
children=as.data.frame(lapply(children,function(x) x[,drop=TRUE])) ## drop unsused levels

## Exclude attention_getters
children=children[children$MediaName!="attngetter169",]
children=as.data.frame(lapply(children,function(x) x[,drop=TRUE])) ## drop unsused levels

## AOI Analysis & Info
## All coordinates
children$x.topright = children$x.topleft + children$x.length
children$y.topright = children$y.topleft
children$x.bottomleft = children$x.topleft
children$y.bottomleft = children$y.topleft + children$y.length
children$x.bottomright = children$x.topright
children$y.bottomright = children$y.bottomleft

## Looking at AOI (for each data point recorded)
children$include.gaze.x=ifelse(children$GazePointX>children$x.topleft&children$GazePointX<children$x.topright,"yes","no")
children$include.gaze.y=ifelse(children$GazePointY<children$y.bottomleft&children$GazePointY>children$y.topleft,"yes","no")
children$looking_aoi=ifelse(children$include.gaze.x=="yes"&children$include.gaze.y=="yes","yes","no")
children$looking_aoi=ifelse(is.na(children$looking_aoi),"no_data",children$looking_aoi) ## This replaces the NA for "no_data".

## LOOKING AT TARGET
target=c()
participant=c()
trial=c()
interval=c()
trial_number=c()
participants=levels(children$RecordingName)
minha.lista=lapply(participants,function(x) children[children$RecordingName==x,]) ## creates a separate dataset for each participant
for(i in 1:length(minha.lista)){ ## Goes through a file for each participant
	part.i=minha.lista[[i]]
		for(j in sort(unique(part.i$trial.number))){ ## Goes for each trial
			trial1=part.i[part.i$trial.number==j,]
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
			trial1=trial1[trial1$is.target=="yes",] ## TARGET OBJECT
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
			trial1=trial1[order(trial1$RecordingTimestamp,decreasing=F),]
			trial1$time=(trial1$RecordingTimestamp-min(trial1$RecordingTimestamp))/1000
			trial1$breaks=cut(trial1$RecordingTimestamp,breaks=160,labels=1:160) ## breaks trial to intervals of roughly 0.1 seconds
			trial1=trial1[trial1$GazeEventType=="Fixation",]
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
				for(k in unique(trial1$breaks)){
					break1=trial1[trial1$breaks==k,]
					yes=sum(break1[break1$looking_aoi=="yes",]$GazeEventDuration)
					target=append(target,yes)
					participant=append(participant,as.vector(unique(break1$RecordingName)))
					trial=append(trial,as.vector(unique(break1$trial.type)))
					interval=append(interval,as.vector(unique(break1$breaks)))
					trial_number=append(trial_number,as.vector(unique(break1$trial.number)))
				}
		}
}
plot_target=data.frame(participant,trial,trial_number,interval,target)

## Looking at NON_TARGET
non_target=c()
participant=c()
trial=c()
interval=c()
trial_number=c()
participants=levels(children$RecordingName)
minha.lista=lapply(participants,function(x) children[children$RecordingName==x,]) ## creates a separate dataset for each participant
for(i in 1:length(minha.lista)){ ## Goes through a file for each participant
	part.i=minha.lista[[i]]
		for(j in sort(unique(part.i$trial.number))){ ## Goes for each trial
			trial1=part.i[part.i$trial.number==j,]
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
			trial1=trial1[trial1$is.target=="no",] ## DISTRACTOR OBJECT
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
			trial1=trial1[order(trial1$RecordingTimestamp,decreasing=F),]
			trial1$time=(trial1$RecordingTimestamp-min(trial1$RecordingTimestamp))/1000
			trial1$breaks=cut(trial1$RecordingTimestamp,breaks=160,labels=1:160) ## breaks trial to intervals of roughly 0.1 seconds
			trial1=trial1[trial1$GazeEventType=="Fixation",]
			trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
				for(k in unique(trial1$breaks)){
					break1=trial1[trial1$breaks==k,]
					yes=sum(break1[break1$looking_aoi=="yes",]$GazeEventDuration)
					non_target=append(non_target,yes)
					participant=append(participant,as.vector(unique(break1$RecordingName)))
					trial=append(trial,as.vector(unique(break1$trial.type)))
					interval=append(interval,as.vector(unique(break1$breaks)))
					trial_number=append(trial_number,as.vector(unique(break1$trial.number)))
				}
		}
}
plot_distractor=data.frame(participant,trial,trial_number,interval,non_target)
plot_data20=data.frame(plot_target,plot_distractor$non_target)
colnames(plot_data20)=c("part","trial","trial_number","interval","target","distractor")
plot_data20$both=plot_data20$target+plot_data20$distractor
plot_data20=plot_data20[plot_data20$both>0,]
plot_data20$prop=plot_data20$target/(plot_data20$target+plot_data20$distractor)
plot_data20$interval=as.numeric(as.character(plot_data20$interval))
time_course20=aggregate(prop~interval+trial,data=plot_data20,mean)
time_course20$time=time_course20$interval*51
time_course20=time_course20[time_course20$time>3000,]

ggplot(data=time_course20,aes(x=time, y=prop,colour=trial))+geom_line(aes(shape=trial,colour=trial))+theme_bw()+ylim(0,1)+geom_point(aes(shape=trial,colour=trial),size = 2.3)+geom_vline(xintercept=c(5400), linetype="dotted")+xlab("Time in Milliseconds")+ylab("Proportion of Time Looking at the target object")



### VAULT
if(0){
## ADULTS
## Time course graph
part=adult[adult$RecordingName=="Mix_Adult_S07",]
part=as.data.frame(lapply(part,function(x) x[,drop=TRUE])) ## drop unsused levels
trial1=part[part$trial.number==3,]
trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
trial1=trial1[trial1$is.target=="yes",]
trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
trial1=trial1[order(trial1$RecordingTimestamp,decreasing=F),]
trial1$time=(trial1$RecordingTimestamp-min(trial1$RecordingTimestamp))/1000
trial1$breaks=cut(trial1$RecordingTimestamp,breaks=160,labels=1:160) ## breaks trial to intervals of roughly 0.1 seconds
trial1=trial1[trial1$GazeEventType=="Fixation",]
# trial1=trial1[trial1$time>4&trial1$time<8,]
# trial1=as.data.frame(lapply(trial1,function(x) x[,drop=TRUE])) ## drop unsused levels
break1=trial1[trial1$breaks==80,]

yes=sum(break1[break1$looking_inside=="yes",]$GazeEventDuration)
yes.looking=yes
participant=as.vector(unique(break1$RecordingName))
trial=as.vector(unique(break1$trial.type))
interval=as.vector(unique(break1$breaks))
plot_data=data.frame(participant,trial,interval,yes.looking)
}
