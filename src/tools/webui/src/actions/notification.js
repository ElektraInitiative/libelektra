export const SEND_NOTIFICATION = "SEND_NOTIFICATION";

export const sendNotification = (message) => {
  return {
    type: SEND_NOTIFICATION,
    message,
  };
};

export const DISMISS_ERROR = "DISMISS_ERROR";

export const dismissError = () => {
  return { type: DISMISS_ERROR };
};
