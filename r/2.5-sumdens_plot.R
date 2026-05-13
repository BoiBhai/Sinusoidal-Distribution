dev.new(width=8, height=6, noRStudioGD=TRUE)

s.unique = sort(unique(sk.opt[,1]))
k.unique = sort(unique(sk.opt[,2]))

Hel.mat = matrix(
  hel.log,
  nrow=length(k.unique),
  ncol=length(s.unique)
)

cols = colorRampPalette(c("green","red"))(200)

layout(matrix(1:2, ncol=2), widths=c(4,0.8))

par(mar=c(4,4,0,0.5))

image(
  x=s.unique,
  y=k.unique,
  z=Hel.mat,
  col=cols,
  xlab="s",
  ylab="k"
)

grid(col="gray", lty="dotted")

par(mar=c(4,0.25,0,4))

plot(
  NA,
  xlim=c(0,1),
  ylim=range(Hel.mat, na.rm=TRUE),
  xaxt="n",
  yaxt="n",
  xlab="",
  ylab="",
  bty="n"
)

y.seq = seq(
  min(Hel.mat, na.rm=TRUE),
  max(Hel.mat, na.rm=TRUE),
  length.out=200
)

rect(
  0,
  head(y.seq,-1),
  1,
  tail(y.seq,-1),
  col=cols,
  border=NA
)

axis(
  4,
  at=pretty(range(Hel.mat, na.rm=TRUE)),
  las=1
)

mtext("-log10(Hellinger)", side=4, line=2.5)

dev.copy2pdf(file='2.5-sumdens.pdf')

#dev.off()