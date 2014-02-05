package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.Ipv4Mask;

public class Ipv4MaskFromString extends JsonDeserializer<Ipv4Mask> {

    @Override
    public Ipv4Mask deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Ipv4Mask.net(jp.getText());
    }
}
