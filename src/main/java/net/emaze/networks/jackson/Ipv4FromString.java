package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.Ip;

public class Ipv4FromString extends JsonDeserializer<Ip> {

    @Override
    public Ip deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Ip.parse(jp.getText());
    }
}
