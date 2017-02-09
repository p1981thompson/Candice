login()
welcome()

library(devtools)
install_github("p1981thompson/osfr")

#create a new OSF project

main<-create_project(title="test project",description="test project for link to R")

#create a subcomponent.
sub<-create_component(id=main, title="sub folder",category="hypothesis")

x<-rnorm(1000,0,1)
write.csv(x,"test.csv")

df<-upload_file(id=sub, filename = "test.csv")

pdf('density.pdf')
plot(density(x))
dev.off()
fig <- upload_file(id=main,filename="density.pdf")


# move files 

move_file(from=sub, to=main, filename=df)

# delete file

delete_file(id=sub)

download(id="3yh9w") # 

# comment

comment_osf(id=main,txt="this is a test comment")

logout()
cleanup()