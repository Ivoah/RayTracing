FROM openjdk:17
WORKDIR /app
COPY RenderServer.jar .
EXPOSE 5122
ENTRYPOINT ["java", "-jar", "RenderServer.jar"]
