export const SEND_NOTIFICATION = 'SEND_NOTIFICATION'

export const sendNotification = (message) => {
  return {
    type: SEND_NOTIFICATION,
    message,
  }
}
