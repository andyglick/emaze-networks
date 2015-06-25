package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import net.emaze.networks.ipv4.Ipv4Mask;

public class Ipv4MaskToString extends JsonSerializer<Ipv4Mask> {

    @Override
    public void serialize(Ipv4Mask mask, JsonGenerator jg, SerializerProvider sp) throws IOException, JsonProcessingException {
        jg.writeString(mask.toString());
    }
}
