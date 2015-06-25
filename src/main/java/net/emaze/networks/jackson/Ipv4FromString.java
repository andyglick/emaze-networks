package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.ipv4.Ipv4;

public class Ipv4FromString extends JsonDeserializer<Ipv4> {

    @Override
    public Ipv4 deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Ipv4.parse(jp.getText());
    }
}
