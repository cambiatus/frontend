#================
# Build Stage
#================
FROM node:14-alpine as builder

ENV NODE_ENV=production

WORKDIR /build

# Copy all application files
COPY . .

# Remove host stuff
RUN rm -Rf node_modules build elm-stuff

RUN yarn install --production
RUN yarn run build

# Copy to opt
RUN cp -r build/ /opt/frontend

#================
# Deployment Stage
#================
FROM nginx:alpine

# Add bash
RUN apk add --no-cache bash
EXPOSE 80

# Add nginx config
RUN rm /etc/nginx/conf.d/default.conf
COPY docker/nginx.conf /etc/nginx/conf.d/default.conf

COPY --from=builder /opt/frontend /usr/share/nginx/html
WORKDIR /root
COPY ./docker/env.sh .
COPY ./docker/.env .

RUN chmod +x /root/env.sh
CMD ["/bin/bash", "-c", "/root/env.sh && nginx -g \"daemon off;\""]
