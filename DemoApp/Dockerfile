FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["DemoApp/DemoApp.fsproj", "DemoApp/"]
RUN dotnet restore "DemoApp/DemoApp.fsproj"
COPY . .
WORKDIR "/src/DemoApp"
RUN dotnet build "DemoApp.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "DemoApp.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "DemoApp.dll"]
