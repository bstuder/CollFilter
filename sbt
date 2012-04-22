java -Dsbt.boot.directory=project/boot/ -Dsbt.ivy.home=project/ivy2/ -Xms512M -Xmx4000M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=512M -jar `dirname $0`/project/sbt-launch.jar "$@"
