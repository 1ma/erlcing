CREATE TYPE STATION_TYPE AS ENUM
  ('regular', 'electric');

CREATE TABLE stations (
  id      SMALLINT PRIMARY KEY,
  is_open BOOLEAN       NOT NULL,
  type    STATION_TYPE  NOT NULL,
  lat     DECIMAL(9, 6) NOT NULL,
  lng     DECIMAL(9, 6) NOT NULL,
  address TEXT          NOT NULL
);

CREATE TABLE occupancy (
  station_id  SMALLINT    NOT NULL REFERENCES stations (id),
  bikes       SMALLINT    NULL,
  slots       SMALLINT    NULL,
  observed_at TIMESTAMPTZ NOT NULL,

  PRIMARY KEY (station_id, observed_at)
);

CREATE TABLE stations_audit (
  station_id SMALLINT    NOT NULL REFERENCES stations (id),
  old_data   TEXT        NULL,
  new_data   TEXT        NOT NULL,
  changed_at TIMESTAMPTZ NOT NULL
);

CREATE FUNCTION stations_audit_trigger()
  RETURNS TRIGGER AS $$
DECLARE
  old_data TEXT;
  new_data TEXT;
BEGIN
  IF TG_OP = 'INSERT' THEN
    old_data = NULL;
  ELSEIF TG_OP = 'UPDATE' THEN
    old_data = ROW (OLD.*) :: TEXT;
  ELSE
    RAISE EXCEPTION 'This trigger only supports INSERT and UPDATE TG_OPs';
  END IF;

  new_data = ROW (NEW.*) :: TEXT;

  INSERT INTO stations_audit (station_id, old_data, new_data, changed_at) VALUES
    (NEW.id, old_data, new_data, NOW());

  RETURN NEW;
END;
$$
LANGUAGE plpgsql;

CREATE TRIGGER stations_au_trigger
AFTER UPDATE ON stations
FOR EACH ROW EXECUTE PROCEDURE stations_audit_trigger();

CREATE TRIGGER stations_ai_trigger
AFTER INSERT ON stations
FOR EACH ROW EXECUTE PROCEDURE stations_audit_trigger();
