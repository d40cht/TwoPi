export LD_LIBRARY_PATH=bin
java -Xmx12000M -XX:OnOutOfMemoryError="kill -3 %p" -XX:+HeapDumpOnOutOfMemoryError -XX:MaxPermSize=512M -XX:+AggressiveOpts -XX:+DoEscapeAnalysis -XX:+UseCompressedOops -server -jar `dirname $0`/sbt-launch.jar "$@"
